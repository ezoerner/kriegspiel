{-# LANGUAGE NamedFieldPuns #-}
module Controller where

import Chess

import View
import Model

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

promote :: Model
        -> View
        -> Bool         -- ^ hotSeat
        -> PieceType
        -> Coordinates  -- ^ fromPos
        -> Coordinates  -- ^ toPos
        -> (Model, View)
promote model@Model{gameState} view hotSeat pieceType fromPos toPos =
  let
    model' = promoteM model fromPos toPos pieceType
  in
    endTurn model' view hotSeat

endTurn :: Model -> View -> Bool -> (Model, View)
endTurn model@Model{gameState} view hotSeat =
  let
    model' = endTurnM model
    view' = endTurnV view (currentPlayer gameState) hotSeat
  in
    (model', view')