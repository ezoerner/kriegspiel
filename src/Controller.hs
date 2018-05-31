{-# LANGUAGE NamedFieldPuns #-}
module Controller where

import Data.Maybe

import Linear.V2  (V2)
import Chess

import View
import Model
import Options
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
        
dropPiece :: Options -> Model -> View -> V2 Int -> (Model, View)
dropPiece options model@Model{gameState} view@View{bbox,orient,coordsInMotion=Just dragCoords} globalPoint =
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
          endTurn options model{gameState = nextGameState} view
        Nothing ->
          (maybe model checkForPromotion maybeTargetCoordMove, view)
dropPiece _ model view  _ = (model, view) -- nothing in motion

promote :: Model
        -> View
        -> Options
        -> PieceType
        -> Coordinates  -- ^ fromPos
        -> Coordinates  -- ^ toPos
        -> (Model, View)
promote model view options pieceType fromPos toPos =
  let
    model' = promoteM model fromPos toPos pieceType
  in
    endTurn options model' view

endTurn :: Options -> Model -> View -> (Model, View)
endTurn options@Options{hotSeat} model@Model{gameState} view =
  let
    model' = endTurnM options model
    view' = endTurnV view (currentPlayer gameState) hotSeat
  in
    (model', view')
