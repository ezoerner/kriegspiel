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
  , inDrag :: Bool
  }
  
mkPiece pieceType player = Piece pieceType player False False

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
    (('a', 1), Just $ mkPiece Rook White)
  , (('b', 1), Just $ mkPiece Knight White)
  , (('c', 1), Just $ mkPiece Bishop White)
  , (('d', 1), Just $ mkPiece Queen White)
  , (('e', 1), Just $ mkPiece King White)
  , (('f', 1), Just $ mkPiece Bishop White)
  , (('g', 1), Just $ mkPiece Knight White)
  , (('h', 1), Just $ mkPiece Rook White)
  , (('a', 2), Just $ mkPiece Pawn White)
  , (('b', 2), Just $ mkPiece Pawn White)
  , (('c', 2), Just $ mkPiece Pawn White)
  , (('d', 2), Just $ mkPiece Pawn White)
  , (('e', 2), Just $ mkPiece Pawn White)
  , (('f', 2), Just $ mkPiece Pawn White)
  , (('g', 2), Just $ mkPiece Pawn White)
  , (('h', 2), Just $ mkPiece Pawn White)
  , (('a', 8), Just $ mkPiece Rook Black)
  , (('b', 8), Just $ mkPiece Knight Black)
  , (('c', 8), Just $ mkPiece Bishop Black)
  , (('d', 8), Just $ mkPiece Queen Black)
  , (('e', 8), Just $ mkPiece King Black)
  , (('f', 8), Just $ mkPiece Bishop Black)
  , (('g', 8), Just $ mkPiece Knight Black)
  , (('h', 8), Just $ mkPiece Rook Black)
  , (('a', 7), Just $ mkPiece Pawn Black)
  , (('b', 7), Just $ mkPiece Pawn Black)
  , (('c', 7), Just $ mkPiece Pawn Black)
  , (('d', 7), Just $ mkPiece Pawn Black)
  , (('e', 7), Just $ mkPiece Pawn Black)
  , (('f', 7), Just $ mkPiece Pawn Black)
  , (('g', 7), Just $ mkPiece Pawn Black)
  , (('h', 7), Just $ mkPiece Pawn Black)
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
                      
piecesForm :: Engine e => Int -> Board -> M.Map String (Image e) -> V2 Int -> Form e
piecesForm boardSize board assets mousePos = let
    showPlayer player = toLower (head $ show player)
    showPieceType pieceType = fmap toLower $ show pieceType
    chooseImage piece = assets M.! ((showPlayer $ player piece) : "_" ++ (showPieceType $ pieceType piece))
    ssize = squareSize boardSize
    imageDims = V2 ssize ssize
    mkForm piece = image imageDims $ chooseImage piece
    pieceImage Nothing _ _ = blank
    pieceImage (Just piece@Piece {inDrag = True}) _ _ =
      toForm $ center (fromIntegral <$> mousePos) $ collage [mkForm piece]
    pieceImage (Just piece) file rank =
      move (V2 (hOffset ssize file) $ vOffset ssize rank) $ mkForm piece
  in
    toForm $ collage [pieceImage maybePiece file rank |
                        file <- ['a'..'h'],
                        rank <- [1..8],
                        let maybePiece = board ! (file, rank)]
                          
findPiece :: Board -> Int -> V2 Int -> Maybe (BoardSquare, Piece)
findPiece board boardSize point = let
    testPoint = fromIntegral <$> point
    ssize = squareSize boardSize
    pieceInSquare :: (BoardSquare, Maybe Piece) -> Bool
    pieceInSquare (_, Nothing) = False
    pieceInSquare ((file, rank), _) = let
        topLeft = V2 (hOffset ssize file) (vOffset ssize rank)
        bottomRight = fmap (+ ssize) topLeft
        bbox = BBox topLeft bottomRight
      in
        pointIntersects testPoint bbox
    assoc = find pieceInSquare (assocs board)
  in
    case assoc of
      Just (boardSquare, Just piece) -> Just (boardSquare, piece)
      _ -> Nothing

squareSize :: Int -> Double
squareSize boardSize = fromIntegral boardSize / 8

hOffset :: Double -> File -> Double
hOffset squareSize file = fromIntegral (ord file - ord 'a') * squareSize

vOffset :: Double -> Rank -> Double
vOffset squareSize rank = (fromIntegral $ 8 - rank) * squareSize


