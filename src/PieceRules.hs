module PieceRules where

import Board

isLegalMove :: BoardPosition -> BoardPosition -> Piece -> Bool
isLegalMove dragPos toPos piece = True