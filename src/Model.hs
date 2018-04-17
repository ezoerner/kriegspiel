{-# LANGUAGE NamedFieldPuns #-}

module Model where

import           Chess
import           Data.Char (ord)
import qualified Data.Map.Strict as M
import           Data.Maybe (isJust)
import           Data.Maybe.HT (toMaybe)
import           Linear.V2 (V2)

import           Board
import           PieceRules
import           Options

data Model = Model
    { gameState :: !GameState
    , options :: !Options
    , windowDims :: !(V2 Int)
    , mousePos :: !(V2 Int)
    , boardView :: BoardView
    , lastMoveAttempt :: !MoveAttempt
    , pawnTries :: !PawnTries
    , check :: !(Maybe Check)
    , gameOver :: !(Maybe GameOver)
    , scores :: !Scores
    }
    deriving (Show)

 initialModel :: Options -> V2 Int -> Model
initialModel options windowDims = Model
    { gameState = newGame
    , options
    , windowDims
    , mousePos = pure 0
    , boardView = initialBoardView windowDims
    , lastMoveAttempt = Successful
    , pawnTries = empty
    , check = Nothing
    , gameOver = Nothing
    , scores = Scores 0 0
    }

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

data GameOver = Checkmate { winner :: !Player } | Draw DrawReason
    deriving (Show)

data DrawReason = Stalemate | Repetition | InsufficientForce | FiftyMove
    deriving (Show)

checkType :: Check -> CheckType
checkType Check{fromPos = (fromFile, fromRank), toPos = (toFile, toRank)}
    | fromFile == toFile = Vertical
    | fromRank == toRank = Horizontal
    | ord fromFile - ord toFile == fromRank - toRank = LongDiagonal -- TO DO differentiate between long and short diagonal
    | otherwise = KnightCheck

resize :: Model -> V2 Int -> Model
resize model@Model{board} windowDims =
  let
    bbox = calcBoardBBox windowDims
  in
    model {windowDims, boardView=boardView{bbox}}

isGameOver :: GameState -> Bool
isGameOver = (&&) . not . isDraw <*> not . isCheckmate

startDragPiece :: Model -> V2 Int -> Model
startDragPiece model@Model
    { boardView = boardView@BoardView{bbox}
    , gameState
    } globalPoint =
  let
    board = board gameState
    currentPlayer = currentPlayer gameState
    localPoint = toBoardLocal (fromIntegral <$> globalPoint) bbox
    maybeBoardPos = findPositionWithPiece board localPoint currentPlayer
    newBoardView boardPos = boardView{posInMotion = Just boardPos}
  in
    if isGameOver gameState
        then model
        else case maybeBoardPos of
            Nothing -> model
            Just boardPos -> model{boardView = newBoardView boardPos}

dropPiece :: Model -> V2 Int -> (Model, Bool)
dropPiece model@Model
    { board = board@Board{bbox, orient, posInMotion = Just dragPos}
    , gameState = gameState@GameState{next = Right thisPlayer}
    }
    globalPoint =
  let
    localPoint = toBoardLocal (fromIntegral <$> globalPoint) bbox
    maybeTargetPos = toBoardPosition bbox localPoint orient >>=
        \toPos -> isLegalMove dragPos toPos board `toMaybe` toPos
    isLegal = isJust maybeTargetPos
    nextPlayer = if isLegal then otherPlayer thisPlayer else thisPlayer
  in
    (model { gameState = gameState{next = Right nextPlayer}
           , board = dropFromTo board dragPos maybeTargetPos
           }
    , isLegal)
dropPiece model _ = (model, False) -- Game Over or Not in Motion
