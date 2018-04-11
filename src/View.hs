{-# LANGUAGE NamedFieldPuns #-}

module View where

import           Helm.Engine.SDL (SDLEngine)
import           Linear.V2 (V2(V2))
import           Helm.Color
import qualified Helm.Graphics2D.Text as Text
import           Helm.Graphics2D

import           Model
import           Board

overlay :: Color -> BoundingSquare -> GameState -> Form SDLEngine
overlay color BSquare{width, topLeft = (V2 left top)} GameState{next, prev} =
  let
    x = width / 2 + left
    y = top / 2
    sidebarX = width + left + 100
    sidebarY = top + 15
    textHeight = 30
    showMoveAttempt Successful = ""
    showMoveAttempt Illegal = "No"
    showMoveAttempt Impossible = "Hell, No!"
    showNext (Right player) = show player ++ " To Move"
    showNext (Left gameOver) = show gameOver -- TO DO improve
  in
    group [ move (V2 x y) $ text $ Text.height textHeight $
                Text.color color $
                Text.toText $ showNext next
          , move (V2 sidebarX sidebarY) $ text $ Text.height textHeight $
                Text.color color $
                Text.toText $ showMoveAttempt prev
          ]
overlay _ _ _ = blank