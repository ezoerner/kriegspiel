{-# LANGUAGE NamedFieldPuns #-}

module Board where

import           Data.Char (toLower, ord, chr)
import           Foundation (ifThenElse)  
import           Helm.Graphics2D
import           Helm.Asset
import           Helm.Engine (Engine)
import           Linear.V2 (V2(V2))
import           Control.Applicative (pure)
import qualified Data.Map.Strict as M
import           Data.List (map, sortOn)
import           Data.Maybe (fromMaybe)
import           Data.Maybe.HT

data Player = White | Black
  deriving (Eq, Show)
  
data Piece = Piece {
    pieceType :: PieceType
  , player :: Player
  , hasMoved :: Bool
  , inDrag :: Bool
  } deriving (Show, Eq)
  
mkPiece :: PieceType -> Player -> Piece
mkPiece pieceType player = Piece pieceType player False False

-- for this game a bounding box is always square
data BoundingSquare = BSquare {
    width :: Double
  , topLeft :: V2 Double
  } deriving (Show)
  
data PieceType = Pawn | Bishop | Knight | Rook | Queen | King
  deriving (Eq, Show)

type File = Char
type Rank = Int

type BoardPosition = (File, Rank)

type Board = M.Map BoardPosition Piece

data BoardColor = Brown | Gray
  deriving (Eq, Show)

initialPosition :: Board
initialPosition = M.fromList [
    (('a', 1), mkPiece Rook White)
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
calcBoardBBox windowSize = let
    constrainSquare (V2 x y) = x `min` y
    calcBoardSize = constrainSquare . subtract (2 * border)
  in
    BSquare { width = fromIntegral $ calcBoardSize windowSize, topLeft = border}

isOnBoard :: BoardPosition -> Bool
isOnBoard pos = fst pos >= fst minPosition &&
                  snd pos >= snd minPosition &&
                  fst pos <= fst maxPosition &&
                  snd pos <= snd maxPosition

boardForm :: Engine e => Image e -> Image e -> BoundingSquare -> Form e
boardForm lightSquare darkSquare boardBBox = let
    ssize = squareSize boardBBox
    imageDims = V2 ssize ssize
    chooseImage x y = ifThenElse (floor (x + y) `mod` (2 :: Integer) == 0) lightSquare darkSquare
    mkForm x y = image imageDims $ chooseImage x y
  in
    toForm $ collage [move (V2 hOff vOff) $ mkForm x y |
                        x <- [0..7]
                      , y <- [0..7]
                      , let hOff = x * ssize
                      , let vOff = y * ssize]
                      
piecesForm :: Engine e => BoundingSquare -> Board -> M.Map String (Image e) -> V2 Int -> Form e
piecesForm bbox board assets mousePos = let
    showPlayer player = toLower (head $ show player)
    showPieceType pieceType = fmap toLower $ show pieceType
    chooseImage piece = assets M.! ((showPlayer $ player piece) : "_" ++ (showPieceType $ pieceType piece))
    ssize = squareSize bbox
    imageDims = pure ssize
    mkForm piece = image imageDims $ chooseImage piece

    pieceImage _ (Just piece@Piece {inDrag = True}) =
      -- subtract topLeft bbox to convert from global to local coordinates
      move ((fromIntegral <$> mousePos) - topLeft bbox - imageDims / 2) $ mkForm piece
    pieceImage boardPosition (Just piece)  =
      move (toUnitOffset boardPosition * pure ssize) $ mkForm piece

    pieces = [((file, rank), maybePiece) |
      file <- ['a'..'h'],
      rank <- [1..8],
      let maybePiece = board M.!? (file, rank),
      maybePiece /= Nothing]
    sortedPieces = sortOn (fmap inDrag . snd) pieces
    imageCollage = collage $ map (\(boardPos, maybePiece) -> pieceImage boardPos maybePiece) $ sortedPieces
  in
    toForm $ imageCollage
                          
findPositionWithPiece :: BoundingSquare -> Board -> V2 Double -> Maybe BoardPosition
findPositionWithPiece boardBBox board point = let
    maybeBoardPos = toBoardPosition boardBBox point
  in
    maybeBoardPos >>=
      \testPos -> fmap (const testPos) $ board M.!? testPos

dropFromTo :: Board -> BoardPosition -> Maybe BoardPosition -> Board
dropFromTo board fromPos maybeToPos = let
    piece = (board M.! fromPos) {inDrag = False}
    destPos = fromMaybe fromPos maybeToPos
    board' = M.insert destPos piece board
  in
    ifThenElse (fromPos /= destPos) (M.delete fromPos board') board'

toUnitOffset :: BoardPosition -> V2 Double
toUnitOffset (file, rank) = fromIntegral <$> V2 (ord file - ord 'a') (8 - rank)

toBoardPosition :: BoundingSquare -> V2 Double -> Maybe BoardPosition
toBoardPosition bbox (V2 x y) = let
    ssize = squareSize bbox
    tryPos = (chr $ ord 'a' + (floor $ x / ssize), 8 - (floor $ y / ssize))
  in
    isOnBoard tryPos `toMaybe` tryPos

squareSize :: BoundingSquare -> Double
squareSize bbox = width bbox / 8

toBoardLocal :: V2 Double -> BoundingSquare -> V2 Double
toBoardLocal globalV2 bbox = globalV2 - topLeft bbox