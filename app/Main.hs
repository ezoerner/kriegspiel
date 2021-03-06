{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Map.Strict     as M
import           Linear.V2           (V2)
import           Options.Applicative
import           System.Directory
import           System.FilePath     ((</>))

import           Chess               hiding (move)
import           Helm
import qualified Helm.Cmd            as Cmd
import           Helm.Color          as HelmColor
import           Helm.Engine.SDL     (SDLEngine)
import qualified Helm.Engine.SDL     as SDL
import           Helm.Graphics2D
import qualified Helm.Keyboard       as Keyboard
import qualified Helm.Mouse          as Mouse
import qualified Helm.Sub            as Sub
import qualified Helm.Window         as Window

import           Controller
import           Model
import           Options
import           View

data App = App
    { model   :: !Model
    , view    :: !View
    , options :: !Options
    }

data Action = DoNothing
            | ResizeWindow (V2 Int)
            | RotateBoard
            | MoveMouse (V2 Int)
            | StartDrag (V2 Int)
            | Drop (V2 Int)
            | HotSeatNext
            | Promote PieceType

backgroundColor :: HelmColor.Color
backgroundColor = rgb (fromRational 252 / 255)
                      (fromRational 244 / 255)
                      (fromRational 220 / 255)
background :: V2 Double -> Form e
background v2 = move (v2 / 2) $ filled backgroundColor $ rect v2

initial :: Options -> (App, Cmd SDLEngine Action)
initial options = (App initialModel initialView options, Cmd.none)

update :: App -> Action -> (App, Cmd SDLEngine Action)
update app@App{view} (ResizeWindow windowSize) =
    (app{view=resize view windowSize}, Cmd.none)
update app@App{view=view@View{playerState=Playing}} RotateBoard =
    (app{view=rotateBoard view}, Cmd.none)
update app@App{view=view@View{playerState=HotSeatWait}} HotSeatNext =
    (app{view=view{playerState=HotSeatBlank}}, Cmd.none)
update app@App{view=view@View{playerState=HotSeatBlank}} HotSeatNext =
    (app{view=view{playerState=Playing}}, Cmd.none)
update app@App{model=Model{gameState}, view=view@View{playerState=Playing}} (StartDrag globalPoint) =
    (app{view=startDragPiece gameState view globalPoint}, Cmd.none)
update app@App{model=model,
               view=view@View{playerState=Playing},
               options} (Drop globalPoint) =
  let
    (m,v) = dropPiece options model view globalPoint
  in
    (app{model=m, view=v{coordsInMotion=Nothing}}, Cmd.none)
update app@App{model} (MoveMouse mousePos) = (app{model=model{mousePos}}, Cmd.none)
update app@App{options,
               model=model,
               view=view@View{playerState=PromotionPrompt fromPos toPos}} (Promote pieceType) =
  let
    (m, v) = promote model view options pieceType fromPos toPos
  in
    (app{model=m, view=v}, Cmd.none)
update app _ = (app, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch
    [ Window.resizes ResizeWindow
    , Keyboard.presses $ \case
        Keyboard.FKey     -> RotateBoard
        Keyboard.SpaceKey -> HotSeatNext
        Keyboard.QKey     -> Promote Queen
        Keyboard.BKey     -> Promote Bishop
        Keyboard.RKey     -> Promote Rook
        Keyboard.NKey     -> Promote Knight
        _                 -> DoNothing
    , Mouse.downs $ \button loc -> case button of
        Mouse.LeftButton -> StartDrag loc
        _                -> DoNothing
    , Mouse.ups $ \button loc -> case button of
        Mouse.LeftButton -> Drop loc
        _                -> DoNothing
    , Mouse.moves $ \loc -> MoveMouse loc
    ]

mainView
    :: M.Map String (Image SDLEngine)
    -> SDLEngine
    -> App
    -> Graphics SDLEngine
mainView assets _ App{options = optns@Options { gameVariant }, model=Model{..}, view=view@View{windowDims,playerState}}
    = let lightSquare  = assets M.! "square_brown_light"
          darkSquare   = assets M.! "square_brown_dark"
          overlayColor = rgb 0 0 0
          pwnTries Kriegspiel Playing = findPawnTries gameState
          pwnTries _          _       = []
      in  Graphics2D $ collage
              [ background (fromIntegral <$> windowDims)
              , textOverlay overlayColor
                            view
                            gameState
                            lastMoveAttempt
                            playerState
              , move border $ boardForm lightSquare
                                        darkSquare
                                        view
                                        (pwnTries gameVariant playerState)
              , move border $ piecesForm playerState
                                         gameState
                                         optns
                                         view
                                         assets
                                         mousePos
              ]

main :: IO ()
main = runGame =<< execParser opts

runGame :: Options -> IO ()
runGame options = do
    print options
    engine <- SDL.startupWith $ SDL.defaultConfig
        { SDL.windowIsResizable  = True
        , SDL.windowDimensions   = initialWindowDims
        , SDL.windowIsFullscreen = False
        , SDL.windowTitle        = show $ gameVariant options
        }

    imageDir <- (</> "images") <$> getCurrentDirectory
    let assetList =
            [ ("b_bishop_png_withShadow.png", "b_bishop")
            , ("b_king_png_withShadow.png"  , "b_king")
            , ("b_knight_png_withShadow.png", "b_knight")
            , ("b_pawn_png_withShadow.png"  , "b_pawn")
            , ("b_queen_png_withShadow.png" , "b_queen")
            , ("b_rook_png_withShadow.png"  , "b_rook")
            , ("w_bishop_png_withShadow.png", "w_bishop")
            , ("w_king_png_withShadow.png"  , "w_king")
            , ("w_knight_png_withShadow.png", "w_knight")
            , ("w_pawn_png_withShadow.png"  , "w_pawn")
            , ("w_queen_png_withShadow.png" , "w_queen")
            , ("w_rook_png_withShadow.png"  , "w_rook")
            , ("square_gray_light.png"      , "square_gray_light")
            , ("square_gray_dark.png"       , "square_gray_dark")
            , ("square_brown_light.png"     , "square_brown_light")
            , ("square_brown_dark.png"      , "square_brown_dark")
            ]
        loadAssets' [] game loaded = game loaded
        loadAssets' ((file, key) : files) game loaded =
            SDL.withImage engine (imageDir </> file)
                $ \img -> loadAssets' files game (M.insert key img loaded)
        loadAssets files game = loadAssets' files game M.empty

    loadAssets assetList $ \allAssets -> run
        engine
        defaultConfig
        GameLifecycle
            { initialFn       = initial options
            , updateFn        = update
            , subscriptionsFn = subscriptions
            , viewFn          = mainView allAssets engine
            }
