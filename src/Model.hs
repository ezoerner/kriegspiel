{-# LANGUAGE NamedFieldPuns #-}

module Model where

import           Chess
import           Linear.V2 (V2)

import           BoardView
import           ChessUtils
import           Options

data Model = Model
    { gameState :: !GameState
    , options :: !Options
    , windowDims :: !(V2 Int)
    , mousePos :: !(V2 Int)
    , boardView :: BoardView
    , lastMoveAttempt :: !MoveAttempt
    , pawnTries :: ![BoardPosition]
    , maybeCheck :: !(Maybe Check)
    , maybeGameOver :: !(Maybe GameOver)
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
    , pawnTries = []
    , maybeCheck = Nothing
    , maybeGameOver = Nothing
    , scores = Scores 0 0
    }

resize :: Model -> V2 Int -> Model
resize model@Model{boardView} windowDims =
  let
    bbox = calcBoardBBox windowDims
  in
    model {windowDims, boardView=boardView{bbox}}

startDragPiece :: Model -> V2 Int -> Model
startDragPiece model@Model
    { boardView = boardView@BoardView{bbox}
    , gameState
    } globalPoint =
  let
    localPoint = toBoardLocal (fromIntegral <$> globalPoint) bbox
    maybeBoardPos = findPositionWithPiece
        (board gameState)
        boardView
        localPoint
        (currentPlayer gameState)
    newBoardView boardPos = boardView{posInMotion = Just boardPos}
  in
    if isGameOver gameState
        then model
        else case maybeBoardPos of
            Nothing -> model
            Just boardPos -> model{boardView = newBoardView boardPos}

dropPiece :: Model -> V2 Int -> (Model, Bool)
dropPiece model@Model
    { boardView = boardView@BoardView{bbox, orient, posInMotion = Just dragPos}
    , gameState
    }
    globalPoint =
  let
    localPoint = toBoardLocal (fromIntegral <$> globalPoint) bbox
    maybeNext = do
        toPos <- toBoardPosition bbox localPoint orient
        let targetCoordMove = toCoordMove dragPos toPos
        move gameState targetCoordMove
  in
    case maybeNext of
        Just nextGameState ->
            (model{ gameState = nextGameState
                  , boardView = boardView{posInMotion = Nothing}
                  }, True)
        Nothing -> (model{boardView = boardView{posInMotion = Nothing}}, False)
dropPiece model _ = (model, False) -- Nothing in motion
