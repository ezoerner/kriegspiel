{-# LANGUAGE NamedFieldPuns #-}

module Board where

import           Control.Applicative (pure)
import           Data.Char (toLower, ord, chr)
import qualified Data.Map.Strict as M
import           Data.List (map, sortOn)
import           Data.Maybe (fromMaybe, isJust)
import           Data.Maybe.HT (toMaybe)
import           Linear.V2 (V2(V2))

import           Helm.Graphics2D
import           Helm.Asset
import           Helm.Engine (Engine)

data Player = White | Black
    deriving (Eq, Show)

data Piece = Piece
    { pieceType :: !PieceType
    , player :: !Player
    , hasMoved :: !Bool
    , inDrag :: !Bool
    } deriving (Show, Eq)

-- for this game a bounding box is always square
data BoundingSquare = BSquare
    { width :: !Double
    , topLeft :: !(V2 Double)
    } deriving (Show)
  
data PieceType = Pawn | Bishop | Knight | Rook | Queen | King
    deriving (Eq, Show)

data BoardColor = Brown | Gray
    deriving (Eq, Show)

type File = Char
type Rank = Int

type BoardPosition = (File, Rank)

type Board = M.Map BoardPosition Piece
type Subboard = Board   -- filtered Board

initialPosition :: Board
initialPosition = M.fromList
    [ (('a', 1), mkPiece Rook White)
    , (('b', 1), mkPiece Knight White)
    , (('c', 1), mkPiece Bishop White)
    , (('d', 1), mkPiece Queen White)
    , (('e', 1), mkPiece King White)
    , (('f', 1), mkPiece Bishop White)
    , (('g', 1), mkPiece Knight White)
    , (('h', 1), mkPiece Rook White)
    , (('a', 2), mkPiece Pawn White)
    , (('b', 2), mkPiece Pawn White)
    , (('c', 2), mkPiece Pawn White)
    , (('d', 2), mkPiece Pawn White)
    , (('e', 2), mkPiece Pawn White)
    , (('f', 2), mkPiece Pawn White)
    , (('g', 2), mkPiece Pawn White)
    , (('h', 2), mkPiece Pawn White)
    , (('a', 8), mkPiece Rook Black)
    , (('b', 8), mkPiece Knight Black)
    , (('c', 8), mkPiece Bishop Black)
    , (('d', 8), mkPiece Queen Black)
    , (('e', 8), mkPiece King Black)
    , (('f', 8), mkPiece Bishop Black)
    , (('g', 8), mkPiece Knight Black)
    , (('h', 8), mkPiece Rook Black)
    , (('a', 7), mkPiece Pawn Black)
    , (('b', 7), mkPiece Pawn Black)
    , (('c', 7), mkPiece Pawn Black)
    , (('d', 7), mkPiece Pawn Black)
    , (('e', 7), mkPiece Pawn Black)
    , (('f', 7), mkPiece Pawn Black)
    , (('g', 7), mkPiece Pawn Black)
    , (('h', 7), mkPiece Pawn Black)
    ]

minPosition :: BoardPosition
minPosition = ('a', 1)

maxPosition :: BoardPosition
maxPosition = ('h', 8)

border :: Num a => V2 a
border = V2 100 100 

calcBoardBBox :: (Integral a, Ord a) => V2 a -> BoundingSquare
calcBoardBBox windowSize =
  let
    constrainSquare (V2 x y) = x `min` y
    calcBoardSize = constrainSquare . subtract (2 * border)
  in
    BSquare { width = fromIntegral $ calcBoardSize windowSize, topLeft = border}

boardForm :: Engine e => Image e -> Image e -> BoundingSquare -> Player -> Form e
boardForm lightSquare darkSquare boardBBox playerOrient =
  let
    ssize = squareSize boardBBox
    imageDims = V2 ssize ssize
    pivot White = 0
    pivot Black = 1
    chooseImage x y = if floor (x + y) `mod` (2 :: Integer) == pivot playerOrient
                      then lightSquare
                      else darkSquare
    mkForm x y = image imageDims $ chooseImage x y
  in
    toForm $ collage [move (V2 hOff vOff) $ mkForm x y
                        | x <- [0..7]
                        , y <- [0..7]
                        , let hOff = x * ssize
                        , let vOff = y * ssize
                        ]

piecesForm :: Engine e => BoundingSquare -> Board -> M.Map String (Image e) ->
    V2 Int -> Player -> Form e
piecesForm bbox board assets mousePos playerOrient =
  let
    showPlayer player = toLower (head $ show player)
    showPieceType pieceType = toLower <$> show pieceType
    playerName piece = showPlayer $ player piece
    pieceName piece = showPieceType $ pieceType piece
    chooseImage piece = assets M.! (playerName piece : "_" ++ pieceName piece)
    ssize = squareSize bbox
    imageDims = pure ssize
    mkForm piece = image imageDims $ chooseImage piece

    pieceImage _ (Just piece@Piece {inDrag = True}) =
        move (toBoardLocal (fromIntegral <$> mousePos) bbox - imageDims / 2) $ mkForm piece
    pieceImage boardPosition (Just piece)  =
      move (toOffset boardPosition playerOrient ssize) $ mkForm piece

    pieces = [((file, rank), maybePiece) |
      file <- ['a'..'h'],
      rank <- [1..8],
      let maybePiece = board M.!? (file, rank),
      isJust maybePiece]
    sortedPieces = sortOn (fmap inDrag . snd) pieces
    imageCollage = collage $ map (uncurry pieceImage) sortedPieces
  in
    toForm imageCollage

findPositionWithPiece :: BoundingSquare -> Board -> V2 Double -> Player -> Maybe BoardPosition
findPositionWithPiece boardBBox board point playerOrient =
  let
    maybeBoardPos = toBoardPosition boardBBox point playerOrient
  in
    maybeBoardPos >>=
      \testPos -> fmap (const testPos) $ board M.!? testPos

-- either a legal move (Just toPos) or an aborted drag (Nothing)
dropFromTo :: Board -> BoardPosition -> Maybe BoardPosition -> Board
dropFromTo board fromPos maybeToPos =
  let
    legalMove = isJust maybeToPos
    piece = board M.! fromPos
    willHaveMoved = legalMove || hasMoved piece
    destPos = fromMaybe fromPos maybeToPos
    board' = M.insert destPos piece {inDrag = False, hasMoved = willHaveMoved} board
  in
    if legalMove
    then M.delete fromPos board'
    else board'

toOffset :: BoardPosition -> Player -> Double -> V2 Double
toOffset (file, rank) White ssize = (fromIntegral <$> V2 (ord file - ord 'a') (8 - rank)) * pure ssize
toOffset (file, rank) Black ssize = (fromIntegral <$> V2 (ord file - ord 'a') (rank - 1)) * pure ssize

toBoardPosition :: BoundingSquare -> V2 Double -> Player -> Maybe BoardPosition
toBoardPosition bbox (V2 x y) playerOrient = let
    ssize = squareSize bbox
    tryPos White = (chr $ ord 'a' + floor (x / ssize), 8 - floor (y / ssize))
    tryPos Black = (chr $ ord 'a' + floor (x / ssize), floor (y / ssize) + 1)
    thisTryPos = tryPos playerOrient
  in
    isOnBoard thisTryPos `toMaybe` thisTryPos

squareSize :: BoundingSquare -> Double
squareSize bbox = width bbox / 8

toBoardLocal :: V2 Double -> BoundingSquare -> V2 Double
toBoardLocal globalV2 bbox = globalV2 - topLeft bbox

-- Private functions

mkPiece :: PieceType -> Player -> Piece
mkPiece pieceType player = Piece pieceType player False False

isOnBoard :: BoardPosition -> Bool
isOnBoard pos = fst pos >= fst minPosition &&
                snd pos >= snd minPosition &&
                fst pos <= fst maxPosition &&
                snd pos <= snd maxPosition
