
{-# LANGUAGE NamedFieldPuns #-}

module Model where

import           Chess
import           Data.Maybe        ( fromJust )
import           Linear.V2         ( V2 )

import           View
import           ChessUtils
import           Options

data Model = Model
    { gameState :: !GameState
    , playerState :: !PlayerState
    , mousePos :: !(V2 Int)
    , lastMoveAttempt :: !MoveAttempt
    }
    deriving (Show)

initialModel :: Model
initialModel = Model
    { gameState       = newGame
    , playerState     = Playing
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

endTurnM :: Options -> Model -> Model
endTurnM Options{gameVariant, hotSeat} model =
  model{ playerState = if hotSeat && gameVariant == Kriegspiel then HotSeatWait else Playing
       , lastMoveAttempt = Successful
       }
