{-# LANGUAGE NamedFieldPuns #-}

module Model where

import           Chess
import           Data.Maybe (fromMaybe, fromJust)
import           Linear.V2 (V2)

import           BoardView
import           ChessUtils
import           Options

data Model = Model
    { gameState :: !GameState
    , playerState :: !PlayerState
    , options :: !Options
    , windowDims :: !(V2 Int)
    , mousePos :: !(V2 Int)
    , boardView :: !BoardView
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
    , playerState = Playing
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

flipBoard :: Model -> Model
flipBoard model@Model{boardView = boardView@BoardView{orient = Black}}
    = model{boardView = boardView{orient = White}}
flipBoard model@Model{boardView = boardView@BoardView{orient = White}}
    = model{boardView = boardView{orient = Black}}


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
    maybeToPos = toBoardPosition bbox localPoint orient
    maybeTargetCoordMove = toCoordMove dragPos <$> maybeToPos
    maybeNext = maybeTargetCoordMove >>= move gameState
  in
    case maybeNext of
        Just nextGameState ->
            (model{ gameState = nextGameState
                  , boardView = boardView{posInMotion = Nothing}
                  }, True)
        Nothing ->
          let maybeResult = maybeTargetCoordMove >>= \targetCoordMove ->
                if canPromote gameState targetCoordMove
                    then Just (model
                        { playerState = PromotionPrompt dragPos (fromJust maybeToPos)
                        , boardView = boardView{posInMotion = Nothing
                        }}, False)
                    else Nothing
          in
            fromMaybe (model{boardView = boardView{posInMotion = Nothing}}, False) maybeResult
dropPiece model _ = (model, False) -- Nothing in motion

canPromote :: GameState -> String -> Bool
canPromote gameState coordMove = isLegalMove gameState (coordMove ++ "=Q")
