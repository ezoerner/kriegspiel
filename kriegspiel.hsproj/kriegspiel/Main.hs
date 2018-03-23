import Linear.V2 (V2(V2))

import Helm
import Helm.Color
import Helm.Engine.SDL (SDLEngine)
import Helm.Graphics2D

import qualified Helm.Cmd as Cmd
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Sub as Sub

import qualified Data.Map as M
import           System.FilePath ((</>))
import           System.Directory
import qualified Helm.Keyboard as Keyboard

windowDims :: V2 Int
windowDims = V2 640 480

imageDims :: V2 Double
imageDims = V2 650 480

data Action = Idle
data Model = Model

initial :: (Model, Cmd SDLEngine Action)
initial = (Model, Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model _ = (model, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions =  Sub.none

view :: M.Map String (Image SDLEngine) -> Model -> Graphics SDLEngine
view assets current = Graphics2D $ collage [image imageDims $ assets M.! "w_queen"]

main :: IO ()
main = do
  engine <- SDL.startupWith $ SDL.defaultConfig
    { SDL.windowIsResizable = False
    , SDL.windowDimensions = windowDims
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
      , viewFn          = view allAssets
      }