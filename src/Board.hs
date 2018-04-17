{-# LANGUAGE NamedFieldPuns #-}

module Board where

import           Control.Applicative (pure)
import           Control.Monad (guard)
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
    , inMotion :: !Bool
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

data Board = Board
    { positions :: !(M.Map BoardPosition Piece)
    , bbox :: !BoundingSquare
    , orient :: !Player
    , posInMotion :: !(Maybe BoardPosition)
    }
    deriving (Show)

initialBoard :: V2 Int -> Board
initialBoard initialWindowDims = Board
    { positions = initialPositions
    , bbox = calcBoardBBox initialWindowDims
    , orient = White
    , posInMotion = Nothing
    }

initialPositions :: M.Map BoardPosition Piece
initialPositions = M.fromList
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

findPositionWithPiece :: Board -> V2 Double -> Player -> Maybe BoardPosition
findPositionWithPiece Board{positions, bbox, orient} point playerTurn =
  let
    maybeBoardPos = toBoardPosition bbox point orient
  in
    maybeBoardPos >>= \testPos ->
        positions M.!? testPos >>= \piece ->
        guard (player piece == playerTurn) >>
        return testPos

-- result is either a legal move (Just toPos) or an aborted drag (Nothing)
dropFromTo :: Board -> BoardPosition -> Maybe BoardPosition -> Board
dropFromTo board@Board{positions} fromPos maybeToPos =
  let
    legalMove = isJust maybeToPos
    piece = positions M.! fromPos
    willHaveMoved = legalMove || hasMoved piece
    destPos = fromMaybe fromPos maybeToPos
    positions' = M.insert
                    destPos
                    piece{inMotion = False, hasMoved = willHaveMoved}
                    positions
  in
    if legalMove
    then board{positions = M.delete fromPos positions', posInMotion = Nothing}
    else board{positions = positions', posInMotion = Nothing}

toOffset :: BoardPosition -> Player -> Double -> V2 Double
toOffset (file, rank) White ssize =
    (fromIntegral <$> V2 (ord file - ord 'a') (8 - rank)) * pure ssize
toOffset (file, rank) Black ssize =
    (fromIntegral <$> V2 (ord file - ord 'a') (rank - 1)) * pure ssize

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
