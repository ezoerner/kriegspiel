{-# LANGUAGE NamedFieldPuns #-}

module PieceRules where

import Board

isLegalMove :: BoardPosition -> BoardPosition -> Piece -> Bool
isLegalMove (fromFile, fromRank) toPos
    (Piece {pieceType = Pawn, player, hasMoved}) = let
        direction = if player == White then 1 else -1
        legalDests = [(fromFile, fromRank + 1 * direction)]
        legalDests' = if hasMoved
                      then legalDests
                      else (fromFile, fromRank + 2 * direction):legalDests
      in
        toPos `elem` legalDests'
isLegalMove _ _ _ = False