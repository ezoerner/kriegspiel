{-# LANGUAGE NamedFieldPuns #-}

import Data.Char (toLower)
import Linear.V2 (V2(V2))

import           Helm
import qualified Helm.Cmd as Cmd
import           Helm.Color
import           Helm.Engine.SDL (SDLEngine)
import qualified Helm.Engine.SDL as SDL
import           Helm.Graphics2D
import qualified Helm.Keyboard as Keyboard
import qualified Helm.Mouse as Mouse
import qualified Helm.Sub as Sub
import qualified Helm.Window as Window

import qualified Data.Map.Strict as M
import           System.FilePath ((</>))
import           System.Directory
import           Board

data Action = DoNothing |
  ResizeWindow (V2 Int) |
  ToggleBoardColor |
  MoveMouse (V2 Int) |
  StartDrag (V2 Int) |
  Drop (V2 Int)

data Model = Model {
    windowDims :: (V2 Int)
  , boardColor :: BoardColor
  , board :: Board
  , mousePos :: V2 Int
} deriving (Show)

border :: Num a => V2 a
border = V2 100 100 

backgroundColor :: Color
backgroundColor = rgb (fromRational 252/255) (fromRational 244/255) (fromRational 220/255)

background :: (V2 Double) -> Form e
background v2 = move (v2 / 2) $ filled backgroundColor $ rect v2

constrainSquare :: Ord a => V2 a -> a
constrainSquare (V2 x y) = x `min` y

calcBoardSize :: (Num a, Ord a) => V2 a -> a
calcBoardSize windowDims = constrainSquare $ windowDims - 2 * border

calcBoardBBox :: (Integral a, Ord a) => V2 a -> BoundingSquare
calcBoardBBox windowDims = BSquare { side = fromIntegral $ calcBoardSize windowDims,
                                     topLeft = border}

initialWindowDims :: Num a => V2 a
initialWindowDims = V2 640 640

initial :: (Model, Cmd SDLEngine Action)
initial = (Model initialWindowDims Brown initialPosition (V2 0 0), Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model (ResizeWindow windowDims) = (model {windowDims}, Cmd.none)
update model@Model {boardColor = Brown} ToggleBoardColor = (model {boardColor = Gray}, Cmd.none)
update model@Model {boardColor = Gray} ToggleBoardColor = (model {boardColor = Brown},  Cmd.none)
update model@Model {board = board, windowDims} (StartDrag loc) =  let
    maybeBoardPos = findPiece (calcBoardBBox windowDims) board loc
  in
    case maybeBoardPos of
      Nothing -> (model, Cmd.none)
      Just boardPos -> (model {board = M.adjust (\p -> p {inDrag = True}) boardPos board}, Cmd.none)
update model (Drop _) = (model, Cmd.none)
update model (MoveMouse mousePos) = (model {mousePos}, Cmd.none)
update model _ = (model, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch
  [Window.resizes ResizeWindow
  , Keyboard.presses $ \key -> case key of
      Keyboard.BKey -> ToggleBoardColor
      _             -> DoNothing
  , Mouse.downs $ \button loc -> case button of
      Mouse.LeftButton -> StartDrag loc
      _                -> DoNothing
  , Mouse.ups $ \button loc -> case button of
      Mouse.LeftButton -> Drop loc
      _                -> DoNothing
  , Mouse.moves $ \loc -> MoveMouse loc
  ]


view :: M.Map String (Image SDLEngine) -> SDLEngine -> Model -> Graphics SDLEngine
view assets _ Model {
    windowDims
  , boardColor
  , board
  , mousePos} = let
    showBoardColor = fmap toLower (show boardColor)
    lightSquare = assets M.! ("square_" ++ showBoardColor ++ "_light")
    darkSquare = assets M.! ("square_" ++ showBoardColor ++ "_dark")
    boardSize = calcBoardSize windowDims
  in
    Graphics2D $ collage [
      background (fromIntegral <$> windowDims)
      , move border $ boardForm lightSquare darkSquare boardSize
      , move border $ piecesForm (calcBoardBBox windowDims) board assets mousePos]

main :: IO ()
main = do
  engine <- SDL.startupWith $ SDL.defaultConfig
    { SDL.windowIsResizable = True
    , SDL.windowDimensions = initialWindowDims
    , SDL.windowIsFullscreen = False
    , SDL.windowTitle = "Kriegspiel"
    }
    
  imageDir <- (</> "images") <$> getCurrentDirectory
  let assetList = [  ("b_bishop_png_withShadow.png", "b_bishop")
                   , ("b_king_png_withShadow.png", "b_king")
                   , ("b_knight_png_withShadow.png", "b_knight")
                   , ("b_pawn_png_withShadow.png", "b_pawn")
                   , ("b_queen_png_withShadow.png", "b_queen")
                   , ("b_rook_png_withShadow.png", "b_rook")
                   , ("w_bishop_png_withShadow.png", "w_bishop")
                   , ("w_king_png_withShadow.png", "w_king")
                   , ("w_knight_png_withShadow.png", "w_knight")
                   , ("w_pawn_png_withShadow.png", "w_pawn")
                   , ("w_queen_png_withShadow.png", "w_queen")
                   , ("w_rook_png_withShadow.png", "w_rook")
                   , ("square_gray_light.png", "square_gray_light")
                   , ("square_gray_dark.png", "square_gray_dark")
                   , ("square_brown_light.png", "square_brown_light")
                   , ("square_brown_dark.png", "square_brown_dark")
                  ]
      loadAssets' [] game loaded = game loaded
      loadAssets' ((file, key):files) game loaded = do
        SDL.withImage engine (imageDir </> file) $ \img ->
          loadAssets' files game (M.insert key img loaded)
      loadAssets files game = loadAssets' files game M.empty

  loadAssets assetList $ \allAssets ->
    run engine defaultConfig GameLifecycle
      { initialFn       = initial
      , updateFn        = update
      , subscriptionsFn = subscriptions
      , viewFn          = view allAssets engine
      }
      