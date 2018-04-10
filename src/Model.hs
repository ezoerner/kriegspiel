{-# LANGUAGE NamedFieldPuns #-}

module Model where

import qualified Data.Map.Strict as M
import           Data.Maybe.HT (toMaybe)
import           Linear.V2 (V2)

import           Board
import           PieceRules

data Model = Model
    { windowDims :: !(V2 Int)
    , boardBBox :: !BoundingSquare
    , boardColor :: !BoardColor
    , boardOrient :: !Player
    , board :: !Board
    , mousePos :: !(V2 Int)
    , posInDrag :: !(Maybe BoardPosition)
    , gameState :: !GameState
    } deriving (Show)

data GameState = GameState
    { toMove :: !(Maybe Player)
    , pawnTries :: !PawnTries
    , pieceGone :: !(Maybe Piece)
    , attemptedMoveIllegal :: !Bool
    , attemptedMoveImpossible :: !Bool
    , check :: !(Maybe Check)
    , gameOver :: !(Maybe GameOver)
    } deriving (Show)

mkGameState :: Player -> GameState
mkGameState player =
    GameState (Just player) M.empty Nothing False False Nothing Nothing

initialGameState :: GameState
initialGameState = mkGameState White

data Check = Check
    { fromPos :: !BoardPosition
    , toPos :: !BoardPosition
    } deriving (Show)

data CheckType = Vertical | Horizontal | LongDiagonal | ShortDiagonal | Knight

data GameOver = Checkmate { winner :: !Player } | Draw DrawReason
    deriving (Show)

data DrawReason = Stalemate | Repetition | InsufficientForce | FiftyMove
    deriving (Show)

checkType :: Check -> CheckType
checkType = undefined

initialModel :: V2 Int -> Model
initialModel initialWindowDims =
  let
    bbox = calcBoardBBox initialWindowDims
  in
    Model
        initialWindowDims
        bbox
        Brown
        White
        initialPosition
        (pure 0)
        Nothing
        initialGameState

resize :: Model -> V2 Int -> Model
resize model windowDims = let
    boardBBox = calcBoardBBox windowDims
  in
    model {windowDims, boardBBox}

startDragPiece :: Model -> V2 Int -> Model
startDragPiece model@Model{boardBBox, board, boardOrient} globalPoint =
  let
    localPoint = toBoardLocal (fromIntegral <$> globalPoint) boardBBox
    maybeBoardPos = findPositionWithPiece boardBBox board localPoint boardOrient
    putInDrag p = p {inDrag = True}
    newBoard boardPos = M.adjust putInDrag boardPos board
  in
    case maybeBoardPos of
      Nothing -> model
      Just boardPos -> model
          { board = newBoard boardPos
          , posInDrag = Just boardPos
          }

dropPiece :: Model -> V2 Int -> Player -> Model
dropPiece model@Model{boardBBox, board, posInDrag = Just dragPos}
          globalPoint playerOrient =
  let
    localPoint = toBoardLocal (fromIntegral <$> globalPoint) boardBBox
    maybeTargetPos = toBoardPosition boardBBox localPoint playerOrient >>=
        \toPos -> isLegalMove dragPos toPos board `toMaybe` toPos
  in
    model {board = dropFromTo board dragPos maybeTargetPos, posInDrag = Nothing}
dropPiece model _ _ = model
