module Transformer.Functions where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State.Lazy (runStateT)
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.Config.Exports as Config
import qualified System.Console.ANSI as Color
import qualified Transformer.Internal as Internal
import Transformer.Types ( Transformer(getTransformer) )
import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( forConcurrently_ )

run :: Show a => Transformer a -> IO ()
run m = do
  let logSettings = Log.Settings Color.Cyan True "runT"
  econfig <- runExceptT (Config.readConfig :: ExceptT Error.Error IO Config.Config)
  case econfig of
    Left e -> do
      let defaultLogConfig = Log.defaultConfig
      Log.critical defaultLogConfig logSettings "Error config read while run the transfomer:"
      Log.critical defaultLogConfig logSettings $ show e
    Right config -> if Config.forks config 
      then forConcurrently_ (zip [1,2..] (Internal.configToStates config)) $ \(i, state) -> do
        threadDelay (i*1000000)
        action state
      else action (Internal.configToState config)      
      where
      action state' = do
        let logConfig = Config.log config
        etuple <- runExceptT $ runStateT (getTransformer m) state'
        case etuple of
          Left e -> do
            Log.error logConfig logSettings "Application error: "
            Log.error logConfig logSettings $ show e
          Right (a, _) -> do
            Log.info logConfig logSettings "Result: "
            Log.info logConfig logSettings $ show a