{-# LANGUAGE NamedFieldPuns #-}
module Controller where

import Data.Maybe

import Linear.V2  (V2)
import Chess

import View
import Model
import ChessUtils

startDragPiece :: GameState -> View -> V2 Int -> View
startDragPiece gameState view@View{bbox} globalPoint =
  let
    localPoint  = toBoardLocal (fromIntegral <$> globalPoint) bbox
    maybeCoords = findPositionWithPiece (board gameState)
                      view
                      localPoint
                      (currentPlayer gameState)
    newView coords = view { coordsInMotion = Just coords }
  in
    if isGameOver gameState
    then view
    else case maybeCoords of
        Nothing     -> view
        Just coords -> newView coords
        
dropPiece :: Model -> View -> V2 Int -> Bool -> (Model, View)
dropPiece model@Model{gameState} view@View{bbox,orient,coordsInMotion=Just dragCoords} globalPoint hotSeat =
    let
      localPoint    = toBoardLocal (fromIntegral <$> globalPoint) bbox
      maybeToCoords = pointToCoords bbox localPoint orient
      maybeTargetCoordMove =
        (\to -> printMove dragCoords to Nothing) <$> maybeToCoords
      maybeNextState = maybeTargetCoordMove >>= move gameState
      wasToSameSquare = dragCoords `elem` maybeToCoords
      pc = fromJust $ pieceAt (board gameState) $ printCoordinate dragCoords
      promotionModel = model
        { playerState = PromotionPrompt dragCoords
                        (fromJust maybeToCoords)
        }
      illegalMoveModel = model{lastMoveAttempt = Illegal pc dragCoords maybeToCoords}
      checkForPromotion targetMove
        | canPromote gameState targetMove = promotionModel
        | wasToSameSquare                 = model
        | otherwise                       = illegalMoveModel
    in
      case maybeNextState of
        Just nextGameState ->
          endTurn model{gameState = nextGameState} view hotSeat
        Nothing ->
          (maybe model checkForPromotion maybeTargetCoordMove, view)

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
