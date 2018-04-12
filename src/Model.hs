{-# LANGUAGE NamedFieldPuns #-}

module Model where

import           Data.Char (ord)
import qualified Data.Map.Strict as M
import           Data.Maybe (isJust)
import           Data.Maybe.HT (toMaybe)
import           Linear.V2 (V2)

import           Board
import           PieceRules

data Model = Model
    { windowDims :: !(V2 Int)
    , board :: !Board
    , mousePos :: !(V2 Int)
    , gameState :: !GameState
    } deriving (Show)

data GameState = GameState
    { next :: !(Either GameOver Player)
    , prev :: !MoveAttempt
    , pawnTries :: !PawnTries
    , pieceGone :: !(Maybe Piece)
    , check :: !(Maybe Check)
    , scores :: Scores
    } deriving (Show)

initialGameState :: GameState
initialGameState = 
    GameState (Right White) Successful M.empty Nothing Nothing (Scores 0 0)

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

initialModel :: V2 Int -> Model
initialModel initialWindowDims = Model
    { windowDims = initialWindowDims
    , board = initialBoard initialWindowDims
    , mousePos = pure 0
    , gameState = initialGameState
    }

resize :: Model -> V2 Int -> Model
resize model@Model{board} windowDims =
  let
    bbox = calcBoardBBox windowDims
  in
    model {windowDims, board=board{bbox}}

startDragPiece :: Model -> V2 Int -> Model
startDragPiece model@Model
    { board = board@Board{bbox, positions}
    , gameState = GameState{next = Right playerTurn}
    } globalPoint =
  let
    localPoint = toBoardLocal (fromIntegral <$> globalPoint) bbox
    maybeBoardPos = findPositionWithPiece board localPoint playerTurn
    putInDrag p = p {inMotion = True}
    newBoard boardPos = board
        { posInMotion = Just boardPos
        , positions = M.adjust putInDrag boardPos positions
        }
  in
    case maybeBoardPos of
      Nothing -> model
      Just boardPos -> model{board = newBoard boardPos}
startDragPiece model _ = model  -- Game Over

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
