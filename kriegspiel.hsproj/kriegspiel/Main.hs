import Helm
import Helm.Engine.SDL (SDLEngine)
import Helm.Graphics2D

import qualified Helm.Cmd as Cmd
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Sub as Sub

import qualified Data.Map as M

data Action = Idle
data Model = Model

initial :: (Model, Cmd SDLEngine Action)
initial = (Model, Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model _ = (model, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions =  Sub.none

view :: Model -> Graphics SDLEngine
view _ = Graphics2D $ collage []

main :: IO ()
main = do
  engine <- SDL.startup

  run engine defaultConfig GameLifecycle
    { initialFn       = initial
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }