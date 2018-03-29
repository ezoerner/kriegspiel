module Board (Board, BoardColor(Brown, Gray), initialPosition, boardForm, piecesForm, findPiece) where

import           Data.Char (toLower, ord)
import           Foundation (ifThenElse)  
import           Helm.Graphics2D
import           Helm.Asset
import           Helm.Engine (Engine)
import           Linear.V2 (V2(V2))
import           Data.Array
import qualified Data.Map as M
import           Data.List (find)
import           Control.Monad (join)

data Player = White | Black
  deriving (Eq, Show)
  
data Piece = Piece {
    pieceType :: PieceType
  , player :: Player
  , hasMoved :: Bool
  }

data BoundingBox = BBox {
    topLeft :: V2 Double
  , bottomRight :: V2 Double
  }
  
pointIntersects :: V2 Double -> BoundingBox -> Bool
pointIntersects (V2 x y) (BBox (V2 topLeftX topLeftY) (V2 btmRightX btmRightY)) =
  x >= topLeftX && x <= btmRightX && y >= topLeftY && y <= btmRightY

data PieceType = Pawn | Bishop | Knight | Rook | Queen | King
  deriving (Eq, Show)

type File = Char
type Rank = Int

type BoardSquare = (File, Rank)

type Board = Array BoardSquare (Maybe Piece)

data BoardColor = Brown | Gray
  deriving (Eq, Show)

initialPosition :: Board
initialPosition = array (('a', 1),('h', 8)) $ [
    (('a', 1), Just $ Piece Rook White False)
  , (('b', 1), Just $ Piece Knight White False)
  , (('c', 1), Just $ Piece Bishop White False)
  , (('d', 1), Just $ Piece Queen White False)
  , (('e', 1), Just $ Piece King White False)
  , (('f', 1), Just $ Piece Bishop White False)
  , (('g', 1), Just $ Piece Knight White False)
  , (('h', 1), Just $ Piece Rook White False)
  , (('a', 2), Just $ Piece Pawn White False)
  , (('b', 2), Just $ Piece Pawn White False)
  , (('c', 2), Just $ Piece Pawn White False)
  , (('d', 2), Just $ Piece Pawn White False)
  , (('e', 2), Just $ Piece Pawn White False)
  , (('f', 2), Just $ Piece Pawn White False)
  , (('g', 2), Just $ Piece Pawn White False)
  , (('h', 2), Just $ Piece Pawn White False)
  , (('a', 8), Just $ Piece Rook Black False)
  , (('b', 8), Just $ Piece Knight Black False)
  , (('c', 8), Just $ Piece Bishop Black False)
  , (('d', 8), Just $ Piece Queen Black False)
  , (('e', 8), Just $ Piece King Black False)
  , (('f', 8), Just $ Piece Bishop Black False)
  , (('g', 8), Just $ Piece Knight Black False)
  , (('h', 8), Just $ Piece Rook Black False)
  , (('a', 7), Just $ Piece Pawn Black False)
  , (('b', 7), Just $ Piece Pawn Black False)
  , (('c', 7), Just $ Piece Pawn Black False)
  , (('d', 7), Just $ Piece Pawn Black False)
  , (('e', 7), Just $ Piece Pawn Black False)
  , (('f', 7), Just $ Piece Pawn Black False)
  , (('g', 7), Just $ Piece Pawn Black False)
  , (('h', 7), Just $ Piece Pawn Black False)
  ] ++ [((file, rank), Nothing) | file <- ['a'..'h'], rank <- [3..6]]

boardForm :: Engine e => Image e -> Image e -> Int -> Form e
boardForm lightSquare darkSquare boardSize = let
    ssize = squareSize boardSize
    imageDims = V2 ssize ssize
    chooseImage x y = ifThenElse (floor (x + y) `mod` 2 == 0) lightSquare darkSquare
    mkForm x y = image imageDims $ chooseImage x y
  in
    toForm $ collage [move (V2 hOffset vOffset) $ mkForm x y |
                        x <- [0..7]
                      , y <- [0..7]
                      , let hOffset = x * ssize
                      , let vOffset = y * ssize]
                      
piecesForm :: Engine e => Int -> Board -> M.Map String (Image e) -> Form e
piecesForm boardSize board assets = let
    showPlayer player = toLower (head $ show player)
    showPieceType pieceType = fmap toLower $ show pieceType
    chooseImage piece = assets M.! ((showPlayer $ player piece) : "_" ++ (showPieceType $ pieceType piece))
    ssize = squareSize boardSize
    imageDims = V2 ssize ssize
    mkForm piece = image imageDims $ chooseImage piece
    pieceImage Nothing _ _ = blank
    pieceImage (Just piece) file rank = move (V2 (hOffset ssize file) $ vOffset ssize rank) $ mkForm piece
  in
    toForm $ collage [pieceImage maybePiece file rank |
                        file <- ['a'..'h'],
                        rank <- [1..8],
                        let maybePiece = board ! (file, rank)]
                        
findPiece :: Board -> Int -> V2 Int -> Maybe Piece
findPiece board boardSize point = let
    testPoint = fromIntegral <$> point
    ssize = squareSize boardSize
    pieceInSquare :: (BoardSquare, Maybe Piece) -> Bool
    pieceInSquare ((file, rank), maybePiece) = let
        topLeft = V2 (hOffset ssize file) (vOffset ssize rank)
        bottomRight = fmap (+ ssize) topLeft
        bbox = BBox topLeft bottomRight
      in
        pointIntersects testPoint bbox
    assoc = find pieceInSquare (assocs board)
  in
    join $ fmap snd assoc

squareSize :: Int -> Double
squareSize boardSize = fromIntegral boardSize / 8

hOffset :: Double -> File -> Double
hOffset squareSize file = fromIntegral (ord file - ord 'a') * squareSize

vOffset :: Double -> Rank -> Double
vOffset squareSize rank = (fromIntegral $ 8 - rank) * squareSize


