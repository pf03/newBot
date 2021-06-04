module Transformer.Internal where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State.Lazy (StateT (runStateT), void)
import Class (MIOError)
import qualified Interface.Error.Exports as Error
import qualified Interface.Log.Exports as Log
import qualified Interface.Cache.Config.Exports as Config
import qualified System.Console.ANSI as Color
import Transformer.Instances (State)
import Transformer.Types (Transformer (getTransformer))

runConfig :: (MIOError m) => m Config.Config
runConfig = do
  config <- Error.catch Config.readConfig $ \err -> do
    Log.critical Log.defaultConfig logSettings "Error config read while run the transfomer:"
    Log.critical Log.defaultConfig logSettings $ show err
    Error.throw err
  let logConfig = Config.log config
  Log.info logConfig logSettings "Config read successfully..."
  return config

showValue :: Show a => Config.Config -> State -> Transformer a -> ExceptT Error.Error IO ()
showValue config state m = do
  let logConfig = Config.log config
  (a, _) <- Error.catch (runStateT (getTransformer m) state) $ \err -> do
    Log.error logConfig logSettings "Application error: "
    Log.error logConfig logSettings $ show err
    Error.throw err
  Log.info logConfig logSettings "Result: "
  Log.info logConfig logSettings $ show a
  return ()

logSettings :: Log.Settings
logSettings = Log.Settings Color.Cyan True "run"

-- Value doesn't matter
runExceptT_ :: ExceptT Error.Error IO () -> IO ()
runExceptT_ m = void (runExceptT m)