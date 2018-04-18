{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import           Data.Char (toLower)
import qualified Data.Map.Strict as M
import           Linear.V2 (V2(V2))
import           Options.Applicative
import           System.FilePath ((</>))
import           System.Directory

import           Helm
import           Helm.Color
import qualified Helm.Cmd as Cmd
import qualified Helm.Engine.SDL as SDL
import           Helm.Engine.SDL (SDLEngine)
import           Helm.Graphics2D
import qualified Helm.Keyboard as Keyboard
import qualified Helm.Mouse as Mouse
import qualified Helm.Sub as Sub
import qualified Helm.Window as Window

import           BoardView
import           Model
import           Options
import           View

data Action = DoNothing
            | ResizeWindow (V2 Int)
            | FlipBoard
            | MoveMouse (V2 Int)
            | StartDrag (V2 Int)
            | Drop (V2 Int)

backgroundColor :: Color
backgroundColor =
    rgb (fromRational 252/255) (fromRational 244/255) (fromRational 220/255)

background :: V2 Double -> Form e
background v2 = move (v2 / 2) $ filled backgroundColor $ rect v2

initialWindowDims :: V2 Int
initialWindowDims = V2 800 640

initial :: Options -> (Model, Cmd SDLEngine Action)
initial options = (initialModel options initialWindowDims, Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model (ResizeWindow windowSize) = (resize model windowSize, Cmd.none)
update model@Model{board = board@Board{orient = White}} FlipBoard
    = (model{board = board{orient = Black}}, Cmd.none)
update model@Model{board = board@Board{orient = Black}} FlipBoard
    = (model{board = board{orient = White}}, Cmd.none)
update model (StartDrag globalPoint) = (startDragPiece model globalPoint, Cmd.none)
update model@Model{options = Options{hotSeat}} (Drop globalPoint) =
  let
    (model', isLegal) = dropPiece model globalPoint
    model'' = if hotSeat && isLegal then fst $ update model' FlipBoard else model'
  in
    (model'', Cmd.none)
update model (MoveMouse mousePos) = (model {mousePos}, Cmd.none)
update model _ = (model, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch
    [ Window.resizes ResizeWindow
    , Keyboard.presses $ \case
        Keyboard.FKey -> FlipBoard
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
view assets _ model@Model{board = board@Board{..}, ..} =
  let
    showBoardColor = fmap toLower (show Brown)
    lightSquare = assets M.! ("square_" ++ showBoardColor ++ "_light")
    darkSquare = assets M.! ("square_" ++ showBoardColor ++ "_dark")
    overlayColor = rgb 0 0 0
  in
    Graphics2D $ collage
        [ background (fromIntegral <$> windowDims)
        , overlay overlayColor bbox gameState
        , move border $ boardForm lightSquare darkSquare board
        , move border $ piecesForm model assets mousePos
        ]

main :: IO ()
main = runGame =<< execParser opts

runGame :: Options -> IO ()
runGame options = do
    print options
    engine <- SDL.startupWith $ SDL.defaultConfig
        { SDL.windowIsResizable = True
        , SDL.windowDimensions = initialWindowDims
        , SDL.windowIsFullscreen = False
        , SDL.windowTitle = show $ gameVariant options
        }

    imageDir <- (</> "images") <$> getCurrentDirectory
    let assetList = [ ("b_bishop_png_withShadow.png", "b_bishop")
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
        loadAssets' ((file, key):files) game loaded =
            SDL.withImage engine (imageDir </> file) $ \img ->
                loadAssets' files game (M.insert key img loaded)
        loadAssets files game = loadAssets' files game M.empty

    loadAssets assetList $ \allAssets ->
        run engine defaultConfig GameLifecycle
            { initialFn       = initial options
            , updateFn        = update
            , subscriptionsFn = subscriptions
            , viewFn          = view allAssets engine
            }
