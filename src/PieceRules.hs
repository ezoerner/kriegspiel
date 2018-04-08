{-# LANGUAGE NamedFieldPuns #-}

module PieceRules where

import qualified Data.Map.Strict as M
import           Data.Maybe (isNothing, fromMaybe)

import Board

isLegalMove :: BoardPosition -> BoardPosition -> Board -> Bool
isLegalMove fromPos toPos board =
  let
    piece = board M.! fromPos
  in
    pieceRule piece fromPos toPos board

pieceRule :: Piece -> BoardPosition -> BoardPosition -> Board -> Bool
    -- Pawn
pieceRule (Piece {pieceType = Pawn, player, hasMoved})
    fromPos@(fromFile, fromRank) toPos board =
  let
    dir = direction player
    legalDests = [(fromFile, fromRank + dir)] -- one step forward
    legalDests' = if hasMoved          -- add two steps forward if hasn't moved
                  then legalDests
                  else (fromFile, fromRank + 2 * dir):legalDests
    legalDests'' = filter (isVacant board) legalDests'  -- but only if vacant
    legalDests''' = legalDests'' ++                     -- add pawn captures
        ([] `fromMaybe` (pawnTries board player M.!? fromPos))
  in
    toPos `elem` legalDests'''
pieceRule _ _ _ _ = False

isVacant :: Board -> BoardPosition -> Bool
isVacant board pos = isNothing $ board M.!? pos

-- Map of fromPos, [toPos]
pawnTries :: Board -> Player -> M.Map BoardPosition [BoardPosition]
pawnTries board thisPlayer =
  let
    dir = direction thisPlayer
    isPawnForThisPlayer (_, piece) = pieceType piece == Pawn
        && player piece == thisPlayer
    pawnAssocs = filter isPawnForThisPlayer $ M.assocs board
    isTry (_, pos:_) = (player <$> board M.!? pos) == Just (otherPlayer thisPlayer)
    triesAt (fromPos@(file, rank), _) = filter isTry [(fromPos, [(succ file, rank + dir)]), (fromPos, [(pred file, rank + dir)])]
  in
    M.fromListWith (++) $ concat $ fmap triesAt pawnAssocs

direction :: Num a => Player -> a
direction player = if player == White then 1 else -1

otherPlayer :: Player -> Player
otherPlayer White = Black
otherPlayer Black = White