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
    , mousePos :: !(V2 Int)
    , lastMoveAttempt :: !MoveAttempt
    }
    deriving (Show)

initialModel :: Options -> V2 Int -> Model
initialModel options windowDims = Model
    { gameState       = newGame
    , playerState     = Playing
    , options
    , mousePos        = pure 0
    , lastMoveAttempt = Successful
    }

promoteM :: Model -> Coordinates -> Coordinates -> PieceType -> Model
promoteM model@Model{gameState} fromPos toPos pieceType =
  let
    coordMove = printMove fromPos toPos $ Just pieceType
  in
    -- application guarantees legal move here
    model{gameState=fromJust $ move gameState coordMove}

canPromote :: GameState -> String -> Bool
canPromote gameState coordMove = isLegalMove gameState (coordMove ++ "=Q")

endTurnM :: Model -> Model
endTurnM model@Model{gameState, options = Options{gameVariant, hotSeat}} =
  model{ playerState = if hotSeat && gameVariant == Kriegspiel then HotSeatWait else Playing
       , lastMoveAttempt = Successful
       }
