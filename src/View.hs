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
overlay color BSquare{width, topLeft = (V2 left top)}
    GameState{next, prev, check} =
  let
    topX = width / 2 + left
    topY = top / 2
    sidebarX = width + left + 100
    sidebarY = top + 15
    textHeight = 30
    showPrev Successful = ""
    showPrev Illegal = "No"
    showPrev Impossible = "Hell, No!"
    showNext (Right player) = " To Move: " ++ show player
    showNext (Left gameOver) = show gameOver -- TO DO improve this
    showCheck (Just LongDiagonal) = "Check on long diagonal"
    showCheck (Just ShortDiagonal) = "Check on short diagonal"
    showCheck (Just KnightCheck) = "Check from a Knight"
    showCheck (Just ckType) = show ckType ++ " check"
    showCheck Nothing = ""
  in
    group [ move (V2 topX topY) $ text $ Text.height textHeight $
                Text.color color $
                Text.toText $ showNext next
          , move (V2 sidebarX sidebarY) $ text $ Text.height textHeight $
                Text.color color $ Text.toText $ showCheck (checkType <$> check)
          , move (V2 sidebarX $ sidebarY + textHeight) $ text $ Text.height textHeight $
                Text.color color $
                Text.toText $ showPrev prev
          ]
