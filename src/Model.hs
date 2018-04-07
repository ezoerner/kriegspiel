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
    , board :: !Board
    , mousePos :: !(V2 Int)
    , posInDrag :: !(Maybe BoardPosition)
    } deriving (Show)
  
initialModel :: V2 Int -> Model
initialModel initialWindowDims =
  let
    bbox = calcBoardBBox initialWindowDims
  in
    Model initialWindowDims bbox Brown initialPosition (pure 0) Nothing

resize :: Model -> V2 Int -> Model
resize model windowDims = let
    boardBBox = calcBoardBBox windowDims
  in
    model {windowDims, boardBBox}

startDragPiece :: Model -> V2 Int -> Model
startDragPiece model@Model{boardBBox, board} globalPoint =
  let
    localPoint = toBoardLocal (fromIntegral <$> globalPoint) boardBBox
    maybeBoardPos = findPositionWithPiece boardBBox board localPoint
    putInDrag = (\p -> p {inDrag = True})
    newBoard boardPos = M.adjust putInDrag boardPos board
  in
    case maybeBoardPos of
      Nothing -> model
      Just boardPos -> model
          { board = newBoard boardPos
          , posInDrag = Just boardPos
          }

dropPiece :: Model -> V2 Int -> Model
dropPiece model@Model{boardBBox, board, posInDrag = Just dragPos}
          globalPoint =
  let
    localPoint = toBoardLocal (fromIntegral <$> globalPoint) boardBBox
    piece = board M.! dragPos
    maybeTargetPos = toBoardPosition boardBBox localPoint >>= \toPos ->
      isLegalMove dragPos toPos piece `toMaybe` toPos
  in
    model {board = dropFromTo board dragPos maybeTargetPos, posInDrag = Nothing}
dropPiece model _ = model
