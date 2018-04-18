{-# LANGUAGE NamedFieldPuns #-}

module PieceRules where

import           Chess (PieceType(..))
import qualified Data.Map.Strict as M
import           Data.Maybe (isNothing, fromMaybe)

import BoardView

type PawnTries = [BoardPosition]

pawnTries :: Board -> Color -> PawnTries
pawnTries Board{positions} thisPlayer =
  let
    dir = direction thisPlayer
    isPawnForThisPlayer (_, piece) = pieceType piece == Pawn
        && player piece == thisPlayer
    pawnAssocs = filter isPawnForThisPlayer $ M.assocs positions
    isTry (_, pos:_) = (player <$> positions M.!? pos) == Just (otherPlayer thisPlayer)
    isTry _ = False -- TODO fix this by considering all the tries not just first one
    triesAt (fromPos@(file, rank), _) =
        filter isTry [
                       (fromPos, [(succ file, rank + dir)])
                     , (fromPos, [(pred file, rank + dir)])
                     ]
  in
    M.fromListWith (++) $ concatMap triesAt pawnAssocs

direction :: Num a => Player -> a
direction player = if player == White then 1 else -1

otherPlayer :: Player -> Player
otherPlayer White = Black
otherPlayer Black = White
