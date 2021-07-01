module Transformer.Internal where

import Class (MIOError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State.Lazy (StateT (runStateT), void)
import qualified Interface.Cache.Config.Exports as Config
import qualified Interface.Error.Exports as Error
import qualified Interface.Log.Exports as Log
import qualified System.Console.ANSI as Color
import qualified Transformer.State as State
import Transformer.Types (Transformer (getTransformer))

runConfig :: (MIOError m) => m Config.Config
runConfig = do
  config <- Error.catch Config.readConfig $ \err -> do
    Log.writeCritical Log.defaultConfig logSettings "Error config read while run the transformer:"
    Log.writeCritical Log.defaultConfig logSettings $ show err
    Error.throw err
  let logConfig = Config.configLog config
  Log.writeInfo logConfig logSettings "Config read successfully..."
  return config

showValue :: Show a => Config.Config -> State.State -> Transformer a -> ExceptT Error.Error IO ()
showValue config state m = do
  let logConfig = Config.configLog config
  (a, _) <- Error.catch (runStateT (getTransformer m) state) $ \err -> do
    Log.writeError logConfig logSettings "Application error: "
    Log.writeError logConfig logSettings $ show err
    Error.throw err
  Log.writeInfo logConfig logSettings "Result: "
  Log.writeInfo logConfig logSettings $ show a
  return ()

logSettings :: Log.Settings
logSettings = Log.Settings Color.Cyan True "run"

-- Value doesn't matter
runExceptT_ :: ExceptT Error.Error IO () -> IO ()
runExceptT_ m = void (runExceptT m)