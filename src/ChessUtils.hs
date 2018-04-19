module ChessUtils where

import Chess
import Data.Char (ord)

type File = Char
type Rank = Int
type BoardPosition = (File, Rank)

isGameOver :: GameState -> Bool
isGameOver = (||) . isDraw <*> isCheckmate

data MoveAttempt = Successful | Illegal | Impossible
    deriving (Show)

data Scores = Scores { white :: Integer, black :: Integer }
    deriving (Show)

data Check = Check
    { fromPos :: !BoardPosition
    , toPos :: !BoardPosition
    } deriving (Show)

data CheckType = Vertical | Horizontal | LongDiagonal | ShortDiagonal | KnightCheck
    deriving (Show)

data GameOver = Checkmate { winner :: !Color } | Draw DrawReason
    deriving (Show)

data DrawReason = Stalemate | Repetition | InsufficientForce | FiftyMove
    deriving (Show)

checkType :: Check -> CheckType
checkType Check{fromPos = (fromFile, fromRank), toPos = (toFile, toRank)}
    | fromFile == toFile = Vertical
    | fromRank == toRank = Horizontal
    | ord fromFile - ord toFile == fromRank - toRank = LongDiagonal -- TO DO differentiate between long and short diagonal
    | otherwise = KnightCheck
