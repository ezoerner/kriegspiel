module Board where

import           Data.Char (toLower, ord)
import           Foundation (ifThenElse)  
import           Helm.Graphics2D
import           Helm.Asset
import           Helm.Engine (Engine)
import           Linear.V2 (V2(V2))
import           Data.Array
import qualified Data.Map as M

data Player = White | Black
  deriving (Eq, Show)
  
data Piece = Pawn | Bishop | Knight | Rook | Queen | King
  deriving (Show, Eq)

type Board = Array (Char, Int) (Maybe (Piece, Player))

initialBoard :: Board
initialBoard = array (('a', 1),('h', 8)) $ [
    (('a', 1), Just (Rook, White))
  , (('b', 1), Just (Knight, White))
  , (('c', 1), Just (Bishop, White))
  , (('d', 1), Just (Queen, White))
  , (('e', 1), Just (King, White))
  , (('f', 1), Just (Bishop, White))
  , (('g', 1), Just (Knight, White))
  , (('h', 1), Just (Rook, White))
  , (('a', 2), Just (Pawn, White))
  , (('b', 2), Just (Pawn, White))
  , (('c', 2), Just (Pawn, White))
  , (('d', 2), Just (Pawn, White))
  , (('e', 2), Just (Pawn, White))
  , (('f', 2), Just (Pawn, White))
  , (('g', 2), Just (Pawn, White))
  , (('h', 2), Just (Pawn, White))
  , (('a', 8), Just (Rook, Black))
  , (('b', 8), Just (Knight, Black))
  , (('c', 8), Just (Bishop, Black))
  , (('d', 8), Just (Queen, Black))
  , (('e', 8), Just (King, Black))
  , (('f', 8), Just (Bishop, Black))
  , (('g', 8), Just (Knight, Black))
  , (('h', 8), Just (Rook, Black))
  , (('a', 7), Just (Pawn, Black))
  , (('b', 7), Just (Pawn, Black))
  , (('c', 7), Just (Pawn, Black))
  , (('d', 7), Just (Pawn, Black))
  , (('e', 7), Just (Pawn, Black))
  , (('f', 7), Just (Pawn, Black))
  , (('g', 7), Just (Pawn, Black))
  , (('h', 7), Just (Pawn, Black))
  ] ++ [((file, rank), Nothing) | file <- ['a'..'h'], rank <- [3..6]]

boardForm :: Engine e => Image e -> Image e -> Int -> Form e
boardForm lightSquare darkSquare boardSize = let
    squareSize = fromIntegral boardSize / 8
    imageDims = V2 squareSize squareSize
    chooseImage x y = ifThenElse ((x + y) `mod` 2 == 0) lightSquare darkSquare
    mkForm x y = image imageDims $ chooseImage x y
  in
    toForm $ collage [move (V2 hOffset vOffset) $ mkForm (floor x) $ floor y |
                        x <- [0..7]
                      , y <- [0..7]
                      , let hOffset = x * squareSize
                      , let vOffset = y * squareSize]
                      
piecesForm :: Engine e => Int -> Board -> M.Map String (Image e) -> Form e
piecesForm boardSize board assets = let
    showPlayer player = toLower (head $ show player)
    showPiece piece = fmap toLower $ show piece
    squareSize = fromIntegral boardSize / 8
    chooseImage (piece, player) = assets M.! ((showPlayer player) : "_" ++ (showPiece piece))
    hOffset file = fromIntegral (ord file - ord 'a') * squareSize
    vOffset rank = (fromIntegral $ 8 - rank) * squareSize
    imageDims = V2 squareSize squareSize
    mkForm man = image imageDims $ chooseImage man
    manImage Nothing _ _ = blank
    manImage (Just man) file rank = move (V2 (hOffset file) $ vOffset rank) $ mkForm man
  in
    toForm $ collage [manImage maybeMan file rank |
                        file <- ['a'..'h'],
                        rank <- [1..8],
                        let maybeMan = board ! (file, rank)]

  

