module Transformer.Functions where

import Interface.Class ( MIOError )
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State.Lazy( MonadIO(liftIO), void, StateT(runStateT) ) --(runStateT)
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.Config.Exports as Config
import qualified System.Console.ANSI as Color
import qualified Transformer.Internal as Internal
import Transformer.Types ( Transformer(getTransformer) )
import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( forConcurrently_ )

run :: Show a => Transformer a -> IO ()
run m = runExceptT_ $ do
  config <- runConfig
  if Config.forks config 
    then do
      let states = Internal.configToStates config
      liftIO $ forConcurrently_ (zip [1,2..] states) $ \(i, state) -> do
        threadDelay (i*1000000)
        runExceptT_ $ showValue config state m
    else do
      let state = Internal.configToState config
      showValue config state m

-----------------------------Run-----------------------------------------------
runConfig :: (MIOError m) => m Config.Config
runConfig = do
    config <- Error.catch Config.readConfig $ \err -> do
        Log.critical Log.defaultConfig logSettings "Error config read while run the transfomer:"
        Log.critical Log.defaultConfig logSettings $ show err
        Error.throw err
    let logConfig = Config.log config
    Log.info logConfig logSettings "Config read successfully..."
    return config

showValue :: Show a => Config.Config -> Internal.State -> Transformer a -> ExceptT Error.Error IO ()
showValue config state m = do
    let logConfig = Config.log config
    (a, _) <-  Error.catch (runStateT (getTransformer m) state) $ \err -> do
        Log.error logConfig logSettings "Application error: "
        Log.error logConfig logSettings $ show err
        Error.throw err
    Log.info logConfig logSettings "Result: "
    Log.info logConfig logSettings $ show a
    return ()

logSettings :: Log.Settings
logSettings = Log.Settings Color.Cyan True "run"

-- Value doesn't matter
runExceptT_ :: ExceptT Error.Error IO () -> IO()
runExceptT_ m = void (runExceptT m)


