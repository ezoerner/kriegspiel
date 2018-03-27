import Data.Char (toLower)
import Linear.V2 (V2(V2))

import Helm
import Helm.Color
import Helm.Engine.SDL (SDLEngine)
import Helm.Graphics2D
-- import Foundation (ifThenElse)

import qualified Helm.Cmd as Cmd
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Sub as Sub

import qualified Data.Map as M
import           System.FilePath ((</>))
import           System.Directory
import qualified Helm.Keyboard as Keyboard
import qualified Helm.Window as Window
import           Board

data Action = DoNothing | ResizeWindow (V2 Int) | ToggleBoardColor
data Model = Model Int BoardColor

data BoardColor = Brown | Gray
  deriving (Eq, Show)

border :: Num a => V2 a
border = V2 50 50 

constrainSquare :: Ord a => V2 a -> a
constrainSquare (V2 x y) = x `min` y

calcBoardSize :: (Num a, Ord a) => V2 a -> a
calcBoardSize windowDims = constrainSquare $ windowDims - 2 * border

initialWindowDims :: V2 Int
initialWindowDims = V2 640 640

initial :: (Model, Cmd SDLEngine Action)
initial = (Model (calcBoardSize initialWindowDims) Brown, Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update (Model _ boardColor) (ResizeWindow newWindowDims) =
  (Model (calcBoardSize newWindowDims) boardColor, Cmd.none)
update (Model boardSize Brown) ToggleBoardColor = (Model boardSize Gray, Cmd.none)
update (Model boardSize Gray) ToggleBoardColor = (Model boardSize Brown, Cmd.none)
update model _ = (model, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch
  [Window.resizes (\(V2 x y) -> ResizeWindow $ V2 x y)
  , Keyboard.presses $ \key -> (case key of
      Keyboard.BKey -> ToggleBoardColor
      _             -> DoNothing)
  ]


view :: M.Map String (Image SDLEngine) -> SDLEngine -> Model -> Graphics SDLEngine
view assets engine (Model boardSize boardColor) = let
    mapToLower = fmap toLower
    lightSquare = assets M.! ("square_" ++ mapToLower (show boardColor) ++ "_light")
    darkSquare = assets M.! ("square_" ++ mapToLower (show boardColor) ++ "_dark")
  in
    Graphics2D $ collage [move border $ Board.form lightSquare darkSquare boardSize]

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
                   , ("square gray light _png.png", "square_gray_light")
                   , ("square gray dark _png.png", "square_gray_dark")
                   , ("square brown light_png.png", "square_brown_light")
                   , ("square brown dark_png.png", "square_brown_dark")
                  ]
      loadAssets' [] game loaded = game loaded
      loadAssets' ((file, id):files) game loaded = do
        SDL.withImage engine (imageDir </> file) $ \image ->
          loadAssets' files game (M.insert id image loaded)
      loadAssets files game = loadAssets' files game M.empty

  loadAssets assetList $ \allAssets ->
    run engine defaultConfig GameLifecycle
      { initialFn       = initial
      , updateFn        = update
      , subscriptionsFn = subscriptions
      , viewFn          = view allAssets engine
      }