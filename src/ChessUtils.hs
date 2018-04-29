module ChessUtils where

import Chess
import Data.Array
import Data.Char
import Data.Maybe

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

opponent :: Color -> Color
opponent White = Black
opponent Black = White

squareThreatenedBy :: Board -> Color -> Coordinates -> Maybe BoardPosition
squareThreatenedBy bord opponentPlayer coords = undefined

isSquareThreatened :: Board -> Color -> Coordinates -> Bool
isSquareThreatened bord opponentPlayer coords = knightsThreaten || pawnsThreaten || otherPiecesThreaten || kingsThreaten || rookOrQueenThreatens || bishopOrQueenThreatens
        where knightSquares = map (sumSquares coords) knightPattern
              knightsThreaten = any isOpponentKnight knightSquares
              isOpponentKnight square = case getPiece bord square of
                                                Just (Piece player Knight) -> player == opponentPlayer
                                                _ -> False
              pawnsThreaten = any isOpponentPawn $ map (sumSquares coords) pawnSquares
              pawnSquares = case opponentPlayer of
                                    White -> [(1, -1), (1, 1)]
                                    Black -> [(-1, -1), (-1, 1)]
              isOpponentPawn square = case getPiece bord square of
                                              Just (Piece player Pawn) -> player == opponentPlayer
                                              _ -> False
              otherPiecesThreaten = False
              kingSquares = map (sumSquares coords) queenPattern
              kingsThreaten = any isOpponentKing kingSquares
              isOpponentKing square  = case getPiece bord square of
                                                Just (Piece player King) -> player == opponentPlayer
                                                _ -> False
              potentialOpponentRookQueenPieces = mapMaybe (firstPieceInSquareList bord . iterateDirectionInsideBoard coords) rookPattern
              rookOrQueenThreatens = any isOpponentRookOrQueen potentialOpponentRookQueenPieces
              isOpponentRookOrQueen (Piece color piecetype) = color == opponentPlayer && piecetype `elem` [Rook, Queen]
              potentialOpponentBishopQueenPieces = mapMaybe (firstPieceInSquareList bord . iterateDirectionInsideBoard coords) bishopPattern
              bishopOrQueenThreatens = any isOpponentBishopOrQueen potentialOpponentBishopQueenPieces
              isOpponentBishopOrQueen (Piece color piecetype) = color == opponentPlayer && piecetype `elem` [Bishop, Queen]


sumSquares :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumSquares (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

getPiece :: Board -> Coordinates -> Maybe Piece
getPiece bord coordinates | inRange (bounds bord) coordinates = f $ bord ! coordinates
                           where f Empty = Nothing
                                 f (Square piece) = Just piece
getPiece _ _ = Nothing

queenPattern :: [(Int, Int)]
queenPattern = rookPattern ++ bishopPattern

rookPattern :: [(Int, Int)]
rookPattern = [(-1, 0), (1, 0), (0, -1), (0, 1)]

bishopPattern :: [(Int, Int)]
bishopPattern = [(-1, -1), (-1, 1), (1, -1), (1, 1)]

knightPattern :: [(Int, Int)]
knightPattern = [(-2, -1), (-1, -2), (1, -2), (2, -1), (-2, 1), (-1, 2), (1, 2), (2, 1)]

firstPieceInSquareList :: Board -> [Coordinates] -> Maybe Piece
firstPieceInSquareList bord coordinates = case firstNonEmpty of
                                                   [] -> Nothing
                                                   (coordinate:_) -> getPiece bord coordinate
        where firstNonEmpty = dropWhile (isEmpty bord) coordinates

isEmpty :: Board -> Coordinates -> Bool
isEmpty bord coordinates = isNothing $ getPiece bord coordinates

iterateDirectionInsideBoard :: Coordinates -> (Int, Int) -> [Coordinates]
iterateDirectionInsideBoard start direction = tail $ takeWhile isInsideBoard $ iterate (sumSquares direction) start

isInsideBoard :: Coordinates -> Bool
isInsideBoard (i, j) = i >= 0 && i <= 7 && j >= 0 && j <= 7
