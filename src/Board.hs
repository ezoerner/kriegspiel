{-# LANGUAGE NamedFieldPuns #-}

module Board (Piece(..), BoundingSquare(..), Board, BoardColor(Brown, Gray), initialPosition, boardForm, piecesForm, findPiece) where

import           Data.Char (toLower, ord)
import           Foundation (ifThenElse)  
import           Helm.Graphics2D
import           Helm.Asset
import           Helm.Engine (Engine)
import           Linear.V2 (V2(V2))
import           Control.Applicative (pure)
import qualified Data.Map.Strict as M
import           Data.List (find)

data Player = White | Black
  deriving (Eq, Show)
  
data Piece = Piece {
    pieceType :: PieceType
  , player :: Player
  , hasMoved :: Bool
  , inDrag :: Bool
  } deriving (Show)
  
mkPiece :: PieceType -> Player -> Piece
mkPiece pieceType player = Piece pieceType player False False

-- for this game a bounding box is always square
data BoundingSquare = BSquare {
    side :: Double
  , topLeft :: V2 Double
  } deriving (Show)
  
pointIntersects :: V2 Double -> BoundingSquare -> Bool
pointIntersects point (BSquare {side, topLeft}) = let
    bottomRight = topLeft + pure side
  in
    --point > topLeft && point < bottomRight
    case (point, topLeft, bottomRight) of
      (V2 px py, V2 orgnX orgnY, V2 cornerX cornerY) ->
        px >= orgnX && px <= cornerX && py >= orgnY && py <= cornerY

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

boardForm :: Engine e => Image e -> Image e -> Int -> Form e
boardForm lightSquare darkSquare boardSize = let
    ssize = squareSize boardSize
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
    boardSize = round $ side bbox
    ssize = squareSize boardSize
    imageDims = V2 ssize ssize
    mkForm piece = image imageDims $ chooseImage piece
    pieceImage Nothing _ _ = blank
    pieceImage (Just piece@Piece {inDrag = True}) _ _ =
      -- subtract topLeft bbox to convert from global to local coordinates
      move ((fromIntegral <$> mousePos) - topLeft bbox - imageDims / 2) $ mkForm piece
    pieceImage (Just piece) file rank =
      move (V2 (hOffset ssize file) $ vOffset ssize rank) $ mkForm piece
  in
    toForm $ collage [pieceImage maybePiece file rank |
                        file <- ['a'..'h'],
                        rank <- [1..8],
                        let maybePiece = board M.!? (file, rank)]
                          
findPiece :: BoundingSquare -> Board -> V2 Int -> Maybe BoardPosition
findPiece boardBBox board point = let
    testPoint = (fromIntegral <$> point) - topLeft boardBBox
    boardSide = round $ side boardBBox
    ssize = squareSize boardSide
    pieceInSquare :: BoardPosition -> Bool
    pieceInSquare (file, rank) = let
        orgn = V2 (hOffset ssize file) (vOffset ssize rank)
        squarebbox = BSquare {side = ssize,  topLeft = orgn}
      in
        pointIntersects testPoint squarebbox
  in
    find pieceInSquare $ M.keys board

squareSize :: Int -> Double
squareSize boardSide = fromIntegral boardSide / 8

hOffset :: Double -> File -> Double
hOffset ssize file = fromIntegral (ord file - ord 'a') * ssize

vOffset :: Double -> Rank -> Double
vOffset ssize rank = (fromIntegral $ 8 - rank) * ssize
