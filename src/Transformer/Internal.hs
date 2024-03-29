module Transformer.Internal where

import Control.Monad.State.Lazy (StateT (runStateT))
import qualified Interface.Cache.Config.Exports as Config
import qualified Interface.Error.Exports as Error
import qualified Interface.Log.Exports as Log
import qualified Transformer.Types as State
import Transformer.Types (BotStateIO)
import Control.Exception ( catch, throwIO )

runConfig :: IO Config.Config
runConfig = do
  config <- catch Config.readConfig $ \err -> do
    Log.writeCritical Log.defaultConfig logSettings "Error config read while run the transformer:"
    Log.writeCritical Log.defaultConfig logSettings $ show err
    throwIO (err :: Error.Error)
  let logConfig = Config.configLog config
  Log.writeInfo logConfig logSettings "Config read successfully..."
  return config

showValue :: Show a => Config.Config -> State.BotState -> BotStateIO a -> IO ()
showValue config state m = do
  let logConfig = Config.configLog config
  (a, _) <- catch (runStateT m state) $ \err -> do
    Log.writeError logConfig logSettings "Application error: "
    Log.writeError logConfig logSettings $ show err
    throwIO (err :: Error.Error)
  Log.writeInfo logConfig logSettings "Result: "
  Log.writeInfo logConfig logSettings $ show a
  return ()

logAndThrow :: Config.Config -> Error.Error -> IO a
logAndThrow config err = do
  let logConfig = Config.configLog config
  Log.writeError logConfig logSettings $ show err
  throwIO (err :: Error.Error)

logSettings :: Log.Settings
logSettings = Log.Settings Log.CyanScheme True "run"