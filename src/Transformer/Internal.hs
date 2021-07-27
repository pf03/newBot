module Transformer.Internal where

import Control.Monad.State.Lazy (StateT (runStateT))
import qualified Interface.Cache.Config.Exports as Config
import qualified Interface.Error.Exports as Error
import qualified Interface.Log.Exports as Log
import qualified System.Console.ANSI as Color
import qualified Transformer.State as State
import Transformer.Types (Transformer (getTransformer))
import Control.Exception ( throw, catch )

runConfig :: IO Config.Config
runConfig = do
  config <- catch Config.readConfig $ \err -> do
    Log.writeCritical Log.defaultConfig logSettings "Error config read while run the transformer:"
    Log.writeCritical Log.defaultConfig logSettings $ show err
    throw (err :: Error.Error)
  let logConfig = Config.configLog config
  Log.writeInfo logConfig logSettings "Config read successfully..."
  return config

showValue :: Show a => Config.Config -> State.State -> Transformer a -> IO ()
showValue config state m = do
  let logConfig = Config.configLog config
  (a, _) <- catch (runStateT (getTransformer m) state) $ \err -> do
    Log.writeError logConfig logSettings "Application error: "
    Log.writeError logConfig logSettings $ show err
    throw (err :: Error.Error)
  Log.writeInfo logConfig logSettings "Result: "
  Log.writeInfo logConfig logSettings $ show a
  return ()

logSettings :: Log.Settings
logSettings = Log.Settings Color.Cyan True "run"