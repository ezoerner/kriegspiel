{-# LANGUAGE NamedFieldPuns #-}

module BoardView where

import           Chess
import           Control.Applicative (pure)
import           Control.Monad (guard)
import           Data.Char (ord, chr)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, isJust)
import           Data.Maybe.HT (toMaybe)
import           Linear.V2 (V2(V2))

-- for this game a bounding box is always square
data BoundingSquare = BSquare
    { width :: !Double
    , topLeft :: !(V2 Double)
    } deriving (Show)

data BoardColor = Brown | Gray
    deriving (Eq, Show)

type File = Char
type Rank = Int

type BoardPosition = (File, Rank)

data BoardView = BoardView
    { bbox :: !BoundingSquare
    , orient :: !Color
    , posInMotion :: !(Maybe BoardPosition)
    }
    deriving (Show)

initialBoardView :: V2 Int -> BoardView
initialBoardView windowDims =
    BoardView
    { bbox = calcBoardView windowDims
    , orient = White
    , posInMotion = Nothing
    }

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

isOnBoard :: BoardPosition -> Bool
isOnBoard pos = fst pos >= fst minPosition &&
                snd pos >= snd minPosition &&
                fst pos <= fst maxPosition &&
                snd pos <= snd maxPosition
