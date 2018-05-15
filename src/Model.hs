{-# LANGUAGE NamedFieldPuns #-}

module Model where

import           Chess
import           Data.Maybe                     ( fromJust )
import           Linear.V2                      ( V2 )

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
    }
    deriving (Show)

initialModel :: Options -> V2 Int -> Model
initialModel options windowDims = Model
    { gameState       = newGame
    , playerState     = Playing
    , options
    , windowDims
    , mousePos        = pure 0
    , boardView       = initialBoardView windowDims
    , lastMoveAttempt = Successful
    }

resize :: Model -> V2 Int -> Model
resize model@Model { boardView } windowDims =
    let bbox = calcBoardBBox windowDims
    in  model { windowDims, boardView = boardView { bbox } }

rotateBoard :: Model -> Model
rotateBoard model@Model { boardView = boardView@BoardView { orient = Black } }
    = model { boardView = boardView { orient = White } }
rotateBoard model@Model { boardView = boardView@BoardView { orient = White } }
    = model { boardView = boardView { orient = Black } }

startDragPiece :: Model -> V2 Int -> Model
startDragPiece model@Model { boardView = boardView@BoardView { bbox }, gameState } globalPoint
    = let localPoint  = toBoardLocal (fromIntegral <$> globalPoint) bbox
          maybeCoords = findPositionWithPiece (board gameState)
                                              boardView
                                              localPoint
                                              (currentPlayer gameState)
          newBoardView coords = boardView { coordsInMotion = Just coords }
      in  if isGameOver gameState
              then model
              else case maybeCoords of
                  Nothing     -> model
                  Just coords -> model { boardView = newBoardView coords }

dropPiece :: Model -> V2 Int -> Model
dropPiece model@Model { boardView = boardView@BoardView { bbox, orient, coordsInMotion = Just dragCoords }, gameState } globalPoint
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
              model { boardView = boardView { coordsInMotion = Nothing } }
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
endTurn model@Model { options = Options { gameVariant, hotSeat }, boardView = boardView@BoardView { orient } }
    = let gameState'                    = gameState model
          (nextPlayerState, nextOrient) = case (gameVariant, hotSeat) of
              (Kriegspiel, True) -> (HotSeatWait, currentPlayer gameState')
              (Chess     , True) -> (Playing, currentPlayer gameState')
              _                  -> (Playing, orient)
      in  model { playerState     = nextPlayerState
                , boardView       = boardView { orient = nextOrient }
                , lastMoveAttempt = Successful
                }
