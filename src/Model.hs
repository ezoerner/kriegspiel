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
        { windowDims = initialWindowDims
        , boardBBox = bbox
        , boardColor = Brown
        , boardOrient = White
        , board = initialPosition
        , mousePos = pure 0
        , posInDrag = Nothing
        , gameState = initialGameState
        }

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
