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
import           Data.Array
import           System.FilePath ((</>))
import           System.Directory
import qualified Helm.Keyboard as Keyboard
import qualified Helm.Window as Window
import           Board

data Action = DoNothing | ResizeWindow (V2 Int) | ToggleBoardColor

data Model = Model {
    windowDims :: (V2 Int)
  , boardColor :: BoardColor
  , board :: Array (Char, Int) (Maybe (Piece, Player))
}

data BoardColor = Brown | Gray
  deriving (Eq, Show)

border :: Num a => V2 a
border = V2 50 50 

backgroundColor :: Color
backgroundColor = rgb (fromRational 252/255) (fromRational 244/255) (fromRational 220/255)

background :: (V2 Double) -> Form e
background v2 = move (v2 / 2) $ filled backgroundColor $ rect v2

constrainSquare :: Ord a => V2 a -> a
constrainSquare (V2 x y) = x `min` y

calcBoardSize :: (Num a, Ord a) => V2 a -> a
calcBoardSize windowDims = constrainSquare $ windowDims - 2 * border

initialWindowDims :: Num a => V2 a
initialWindowDims = V2 640 640

initial :: (Model, Cmd SDLEngine Action)
initial = (Model initialWindowDims Brown initialBoard, Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model (ResizeWindow newWindowDims) = (model {windowDims = newWindowDims}, Cmd.none)
update model@Model {boardColor = Brown} ToggleBoardColor = (model {boardColor = Gray}, Cmd.none)
update model@Model {boardColor = Gray} ToggleBoardColor = (model {boardColor = Brown}, Cmd.none)
update model _ = (model, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch
  [Window.resizes ResizeWindow
  , Keyboard.presses $ \key -> (case key of
      Keyboard.BKey -> ToggleBoardColor
      _             -> DoNothing)
  ]


view :: M.Map String (Image SDLEngine) -> SDLEngine -> Model -> Graphics SDLEngine
view assets engine Model {windowDims = windDims, boardColor = boardColor} = let
    showBoardColor = fmap toLower (show boardColor)
    lightSquare = assets M.! ("square_" ++ showBoardColor ++ "_light")
    darkSquare = assets M.! ("square_" ++ showBoardColor ++ "_dark")
  in
    Graphics2D $ collage [
      background (fromIntegral <$> windDims)
      , move border $ Board.form lightSquare darkSquare $ calcBoardSize windDims]

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