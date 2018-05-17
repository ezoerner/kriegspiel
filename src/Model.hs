{-# LANGUAGE NamedFieldPuns #-}

module Model where

import           Chess
import           Data.Maybe                     ( fromJust )
import           Linear.V2                      ( V2 )

import           View
import           ChessUtils
import           Options

data Model = Model
    { gameState :: !GameState
    , playerState :: !PlayerState
    , options :: !Options
    , windowDims :: !(V2 Int)
    , mousePos :: !(V2 Int)
    , view :: !View
    , lastMoveAttempt :: !MoveAttempt
    }
    deriving (Show)

initialModel :: Options -> V2 Int -> Model
initialModel options windowDims = Model
    { gameState       = newGame
    , playerState     = Playing
    , options
    , windowDims
    , mousePos        = pure 0
    , view       = initialView windowDims
    , lastMoveAttempt = Successful
    }

resize :: Model -> V2 Int -> Model
resize model@Model { view } windowDims =
    let bbox = calcBoardBBox windowDims
    in  model { windowDims, view = view { bbox } }

rotateBoard :: Model -> Model
rotateBoard model@Model { view = view@View { orient = Black } }
    = model { view = view { orient = White } }
rotateBoard model@Model { view = view@View { orient = White } }
    = model { view = view { orient = Black } }

startDragPiece :: Model -> V2 Int -> Model
startDragPiece model@Model { view = view@View { bbox }, gameState } globalPoint
    = let localPoint  = toBoardLocal (fromIntegral <$> globalPoint) bbox
          maybeCoords = findPositionWithPiece (board gameState)
                                              view
                                              localPoint
                                              (currentPlayer gameState)
          newView coords = view { coordsInMotion = Just coords }
      in  if isGameOver gameState
              then model
              else case maybeCoords of
                  Nothing     -> model
                  Just coords -> model { view = newView coords }

dropPiece :: Model -> V2 Int -> Model
dropPiece model@Model { view = view@View { bbox, orient, coordsInMotion = Just dragCoords }, gameState } globalPoint
    = let
          localPoint    = toBoardLocal (fromIntegral <$> globalPoint) bbox
          maybeToCoords = pointToCoords bbox localPoint orient
          maybeTargetCoordMove =
              (\to -> printMove dragCoords to Nothing) <$> maybeToCoords
          maybeNextState = maybeTargetCoordMove >>= move gameState
          wasToSameSquare = dragCoords `elem` maybeToCoords
          pc = fromJust $ pieceAt (board gameState) $ printCoordinate
              dragCoords
          stopDragModel =
              model { view = view { coordsInMotion = Nothing } }
          promotionModel = stopDragModel
              { playerState = PromotionPrompt dragCoords
                                              (fromJust maybeToCoords)
              }
          illegalMoveModel = stopDragModel
              { lastMoveAttempt = Illegal pc dragCoords maybeToCoords
              }
          checkForPromotion targetMove
              | canPromote gameState targetMove = promotionModel
              | wasToSameSquare                 = stopDragModel
              | otherwise                       = illegalMoveModel
      in
          case maybeNextState of
              Just nextGameState ->
                  endTurn stopDragModel { gameState = nextGameState }
              Nothing ->
                  maybe stopDragModel checkForPromotion maybeTargetCoordMove
dropPiece model _ = model -- Nothing in motion

promote :: Model -> PieceType -> Model
promote model@Model { gameState, playerState = PromotionPrompt fromPos toPos } pieceType
    = let coordMove    = printMove fromPos toPos $ Just pieceType
          newGameState = fromJust $ move gameState coordMove -- state machine guarantees a Just
      in  endTurn $ model { gameState = newGameState }
promote model _ = model -- cannot promote in other player states

canPromote :: GameState -> String -> Bool
canPromote gameState coordMove = isLegalMove gameState (coordMove ++ "=Q")

endTurn :: Model -> Model
endTurn model@Model { options = Options { gameVariant, hotSeat }, view = view@View { orient } }
    = let gameState'                    = gameState model
          (nextPlayerState, nextOrient) = case (gameVariant, hotSeat) of
              (Kriegspiel, True) -> (HotSeatWait, currentPlayer gameState')
              (Chess     , True) -> (Playing, currentPlayer gameState')
              _                  -> (Playing, orient)
      in  model { playerState     = nextPlayerState
                , view       = view { orient = nextOrient }
                , lastMoveAttempt = Successful
                }
