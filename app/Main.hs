{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Map.Strict as M
import           Linear.V2 (V2(V2))
import           Options.Applicative
import           System.FilePath ((</>))
import           System.Directory

import           Chess hiding (move)
import           Helm
import           Helm.Color as HelmColor
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

data Action = DoNothing
            | ResizeWindow (V2 Int)
            | FlipBoard
            | MoveMouse (V2 Int)
            | StartDrag (V2 Int)
            | Drop (V2 Int)
            | HotSeatNext

backgroundColor :: HelmColor.Color
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
update model@Model{playerState = Playing} FlipBoard = (flipBoard model, Cmd.none)
update model@Model{playerState = Playing} HotSeatNext = (model, Cmd.none)
update model@Model{playerState = HotSeatWait} HotSeatNext = (model{playerState = HotSeatBlank}, Cmd.none)
update model@Model{playerState = HotSeatBlank} HotSeatNext = (model{playerState = Playing}, Cmd.none)
update model@Model{playerState = Playing} (StartDrag globalPoint) = (startDragPiece model globalPoint, Cmd.none)
update model@Model{playerState = Playing, options = Options{gameVariant, hotSeat}, boardView} (Drop globalPoint) =
  let
    (model', isLegal) = dropPiece model globalPoint
    gameState' = gameState model'
    nextState = case gameVariant of
        Kriegspiel -> HotSeatWait
        Chess -> Playing
    model'' = if hotSeat && isLegal
              then model'{playerState = nextState, boardView=boardView{orient=currentPlayer gameState'}}
              else model'
  in
    (model'', Cmd.none)
update model (MoveMouse mousePos) = (model {mousePos}, Cmd.none)
update model _ = (model, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch
    [ Window.resizes ResizeWindow
    , Keyboard.presses $ \case
        Keyboard.FKey       -> FlipBoard
        Keyboard.SpaceKey   -> HotSeatNext
        _                   -> DoNothing
    , Mouse.downs $ \button loc -> case button of
        Mouse.LeftButton -> StartDrag loc
        _                -> DoNothing
    , Mouse.ups $ \button loc -> case button of
        Mouse.LeftButton -> Drop loc
        _                -> DoNothing
    , Mouse.moves $ \loc -> MoveMouse loc
    ]

view :: M.Map String (Image SDLEngine) -> SDLEngine -> Model -> Graphics SDLEngine
view assets _ Model{..} =
  let
    lightSquare = assets M.! "square_brown_light"
    darkSquare = assets M.! "square_brown_dark"
    overlayColor = rgb 0 0 0
  in
    Graphics2D $ collage
        [ background (fromIntegral <$> windowDims)
        , overlay overlayColor boardView gameState lastMoveAttempt maybeCheck maybeGameOver playerState
        , move border $ boardForm lightSquare darkSquare boardView
        , move border $ piecesForm playerState gameState options boardView assets mousePos
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
