{-# LANGUAGE NamedFieldPuns #-}

module Model ( Model (..)
             , Check (..)
             , GameOver (..)
             , DrawReason (..)
             , MoveAttempt (..)
             , initialModel
             , promoteM
             , canPromote
             , endTurnM
             , isGameOver
             , maybeGameOver
             , findPawnTries
             , findChecks
             , printMove
             , printCoordinate
             , squareToMaybe
             , isInsideBoard
             ) where

import           Chess
import           Data.Array
import           Data.Char
import           Data.List     (nub)
import           Data.Maybe
import           Data.Maybe.HT (toMaybe)
import           Linear.V2     (V2)

data Model = Model
    { gameState       :: !GameState
    , mousePos        :: !(V2 Int)
    , lastMoveAttempt :: !MoveAttempt
    }
    deriving (Show)

data Check = Vertical | Horizontal | LongDiagonal | ShortDiagonal | KnightCheck
  deriving (Show, Eq)

data GameOver = Checkmate { winnerColor :: !Color } | Draw DrawReason

instance Show GameOver where
  show (Checkmate winColor) = "Checkmate! " ++ show winColor ++ " wins!"
  show (Draw drawReason)    = "Draw due to " ++ show drawReason

data DrawReason = Stalemate | InsufficientForce

data MoveAttempt = Successful | Illegal Piece Coordinates (Maybe Coordinates)
  deriving (Show)

instance Show DrawReason where
  show Stalemate         = "Stalemate"
  show InsufficientForce = "Insufficient Force"

initialModel :: Model
initialModel = Model
    { gameState       = newGame
    , mousePos        = pure 0
    , lastMoveAttempt = Successful
    }

promoteM :: Model -> Coordinates -> Coordinates -> PieceType -> Model
promoteM model@Model{gameState} fromPos toPos pieceType =
  let
    coordMove = printMove fromPos toPos $ Just pieceType
  in
    -- application guarantees legal move here
    model{gameState=fromJust $ move gameState coordMove}

canPromote :: GameState -> String -> Bool
canPromote gameState coordMove = isLegalMove gameState (coordMove ++ "=Q")

endTurnM :: Model -> Model
endTurnM model =
  model{ lastMoveAttempt = Successful }

isGameOver :: GameState -> Bool
isGameOver = isJust . maybeGameOver

maybeGameOver :: GameState -> Maybe GameOver
maybeGameOver gameState
    | isCheckmate gameState = Just $ Checkmate (fromJust $ winner gameState)
    | isStalemate gameState = Just $ Draw Stalemate
    | isDraw gameState && not (isStalemate gameState) = Just
    $ Draw InsufficientForce
    | otherwise = Nothing  -- Draw by Repetition or FiftyMove not implemented by hschesslib library
    -- TODO allow draw by agreement

findPawnTries :: GameState -> [Coordinates]
findPawnTries gameState
    = [ toCoords
      | (fromCoords@(rank, file), square) <- assocs $ board gameState
      , square /= Empty
      , let (Piece clr pieceType) = fromJust (squareToMaybe square)
      , let thisPlayer            = currentPlayer gameState
      , let direction = if thisPlayer == White then -1 else 1
      , pieceType == Pawn
      , clr == thisPlayer
      , toCoords <- [(rank + direction, file - 1), (rank + direction, file + 1)]
      , let moveString = printMove fromCoords toCoords Nothing
      , let moveStringWithSomePromotion =
                printMove fromCoords toCoords $ Just Queen
      , isLegalMove gameState moveString ||
            isLegalMove gameState moveStringWithSomePromotion
      ]

findChecks :: GameState -> [Check]
findChecks gameState
    | isGameOver gameState = []
    | otherwise = nub $ catMaybes
        [ knightsThreaten `toMaybe` KnightCheck
        , anyOnLongDiagonal threateningPawnSquares coords `toMaybe` LongDiagonal
        , anyOnShortDiagonal threateningPawnSquares coords
            `toMaybe` ShortDiagonal
        , anyOnHorizontal threateningRookOrQueenSquares coords
            `toMaybe` Horizontal
        , anyOnVertical threateningRookOrQueenSquares coords `toMaybe` Vertical
        , anyOnLongDiagonal threateningBishopOrQueenSquares coords
            `toMaybe` LongDiagonal
        , anyOnShortDiagonal threateningBishopOrQueenSquares coords
            `toMaybe` ShortDiagonal
        ]
  where
    bord            = board gameState
    currPlayer      = currentPlayer gameState
    coords          = getKingSquare bord currPlayer
    opponentPlayer  = opponent currPlayer
    knightSquares   = map (sumSquares coords) knightPattern
    knightsThreaten = any isOpponentKnight knightSquares
    isOpponentKnight square = case getPiece bord square of
        Just (Piece player Knight) -> player == opponentPlayer
        _                          -> False

    threateningPawnSquares =
        filter isOpponentPawn $ map (sumSquares coords) pawnSquares
    pawnSquares = case opponentPlayer of
        White -> [(1, -1), (1, 1)]
        Black -> [(-1, -1), (-1, 1)]
    isOpponentPawn square = case getPiece bord square of
        Just (Piece player Pawn) -> player == opponentPlayer
        _                        -> False

    potentialOpponentRookQueenPieceSquares = mapMaybe
        (firstPieceInSquareList bord . iterateDirectionInsideBoard coords)
        rookPattern
    threateningRookOrQueenSquares =
        snd
            <$> filter (isOpponentRookOrQueen . fst)
                       potentialOpponentRookQueenPieceSquares
    isOpponentRookOrQueen (Piece color piecetype) =
        color == opponentPlayer && piecetype `elem` [Rook, Queen]

    potentialOpponentBishopQueenPieceSquares = mapMaybe
        (firstPieceInSquareList bord . iterateDirectionInsideBoard coords)
        bishopPattern
    threateningBishopOrQueenSquares =
        snd
            <$> filter (isOpponentBishopOrQueen . fst)
                       potentialOpponentBishopQueenPieceSquares
    isOpponentBishopOrQueen (Piece color piecetype) =
        color == opponentPlayer && piecetype `elem` [Bishop, Queen]

printMove :: Coordinates -> Coordinates -> Maybe PieceType -> String
printMove fromCoords toCoords maybePromotionPieceType =
    let showPieceType Knight   = "N"
        showPieceType pieceTyp = [head $ show pieceTyp]
    in  printCoordinate fromCoords
        ++ "-"
        ++ printCoordinate toCoords
        ++ (case maybePromotionPieceType of
               Nothing        -> ""
               Just pieceType -> "=" ++ showPieceType pieceType
           )

printCoordinate :: Coordinates -> String
printCoordinate (r, c) = [chr (ord 'a' + c), intToDigit (8 - r)]

squareToMaybe :: Square -> Maybe Piece
squareToMaybe Empty          = Nothing
squareToMaybe (Square piece) = Just piece

isInsideBoard :: Coordinates -> Bool
isInsideBoard (i, j) = i >= 0 && i <= 7 && j >= 0 && j <= 7

--------- Private Functions ---------

opponent :: Color -> Color
opponent White = Black
opponent Black = White

anyOnLongDiagonal :: [Coordinates] -> Coordinates -> Bool
anyOnLongDiagonal testCoords start = any (onLongDiagonal start) testCoords

anyOnShortDiagonal :: [Coordinates] -> Coordinates -> Bool
anyOnShortDiagonal testCoords start = any (onShortDiagonal start) testCoords

anyOnHorizontal :: [Coordinates] -> Coordinates -> Bool
anyOnHorizontal testCoords start =
    let isOnHorizontal (rank1, _) (rank2, _) = rank1 == rank2
    in  any (isOnHorizontal start) testCoords

anyOnVertical :: [Coordinates] -> Coordinates -> Bool
anyOnVertical testCoords start =
    let isOnVertical (_, file1) (_, file2) = file1 == file2
    in  any (isOnVertical start) testCoords

onShortDiagonal :: Coordinates -> Coordinates -> Bool
onShortDiagonal start testCoords
    | not (isOnDiagonal start testCoords) = False
    | otherwise = not $ onLongDiagonal start testCoords

onLongDiagonal :: Coordinates -> Coordinates -> Bool
onLongDiagonal start testCoords
    | not (isOnDiagonal start testCoords) = False
    | isCorner start = True
    | otherwise = diagLength diag > diagLength otherDiag
  where
    diag      = expandDiagonal start testCoords
    otherDiag = uncurry expandDiagonal $ otherDiagonal start testCoords

isCorner :: Coordinates -> Bool
isCorner (x, y) = (x == 0 || x == 7) && (y == 0 || y == 7)

diagLength :: (Coordinates, Coordinates) -> Int
diagLength ((x1, _), (x2, _)) = abs (x1 - x2)

otherDiagonal :: Coordinates -> Coordinates -> (Coordinates, Coordinates)
otherDiagonal coords1@(x1, y1) (x2, y2) =
    let (dirX, dirY) = unitDirection (x1, y1) (x2, y2)
        otherCoords  = if isInsideBoard (sumSquares coords1 (-dirX, dirY))
            then sumSquares coords1 (-dirX, dirY)
            else sumSquares coords1 (dirX, -dirY)
    in  expandDiagonal coords1 otherCoords

expandDiagonal :: Coordinates -> Coordinates -> (Coordinates, Coordinates)
expandDiagonal coords1 coords2
    = let
          direction1 = unitDirection coords1 coords2
          endpoint1 =
              last $ iterateDirectionInsideBoardInclusive coords2 direction1
          endpoint2 =
              last
                  $ iterateDirectionInsideBoardInclusive coords1
                  $ oppositeDirection direction1
      in
          (endpoint1, endpoint2)

unitDirection
    :: Coordinates -- ^ from
    -> Coordinates -- ^ to
    -> (Int, Int)
unitDirection (rank1, file1) (rank2, file2) =
    (signum (rank2 - rank1), signum (file2 - file1))

oppositeDirection :: (Int, Int) -> (Int, Int)
oppositeDirection (x, y) = (-x, -y)

isOnDiagonal :: Coordinates -> Coordinates -> Bool
isOnDiagonal (rank1, file1) (rank2, file2) =
    rank2 - rank1 /= 0 && abs (rank2 - rank1) == abs (file2 - file1)

sumSquares :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumSquares (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

getPiece :: Board -> Coordinates -> Maybe Piece
getPiece bord coordinates | inRange (bounds bord) coordinates =
    f $ bord ! coordinates
  where
    f Empty          = Nothing
    f (Square piece) = Just piece
getPiece _ _ = Nothing

getKingSquare :: Board -> Color -> Coordinates
getKingSquare bord player =
    fromJust $ rlookup (Square (Piece player King)) $ assocs bord
  where
    rlookup x = lookup x . map swap
    swap (x, y) = (y, x)

rookPattern :: [(Int, Int)]
rookPattern = [(-1, 0), (1, 0), (0, -1), (0, 1)]

bishopPattern :: [(Int, Int)]
bishopPattern = [(-1, -1), (-1, 1), (1, -1), (1, 1)]

knightPattern :: [(Int, Int)]
knightPattern =
    [(-2, -1), (-1, -2), (1, -2), (2, -1), (-2, 1), (-1, 2), (1, 2), (2, 1)]

firstPieceInSquareList :: Board -> [Coordinates] -> Maybe (Piece, Coordinates)
firstPieceInSquareList bord coordinates = case firstNonEmpty of
    [] -> Nothing
    (coordinate : _) ->
        (\piece -> (piece, coordinate)) <$> getPiece bord coordinate
    where firstNonEmpty = dropWhile (isEmpty bord) coordinates

isEmpty :: Board -> Coordinates -> Bool
isEmpty bord coordinates = isNothing $ getPiece bord coordinates

iterateDirectionInsideBoard :: Coordinates -> (Int, Int) -> [Coordinates]
iterateDirectionInsideBoard start direction =
    tail $ iterateDirectionInsideBoardInclusive start direction

iterateDirectionInsideBoardInclusive
    :: Coordinates -> (Int, Int) -> [Coordinates]
iterateDirectionInsideBoardInclusive start direction =
    takeWhile isInsideBoard $ iterate (sumSquares direction) start
