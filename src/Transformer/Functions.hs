module Transformer.Functions where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State.Lazy (runStateT)
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.Config.Exports as Config
import qualified System.Console.ANSI as Color
import Transformer.Internal as Internal( configToState, configToStates ) 
import Transformer.Types ( Transformer(getTransformer) )
import Control.Concurrent ( threadDelay, forkIO )
import Control.Monad (forM_)

run :: Show a => Transformer a -> IO ()
run m = do
  let settings = Log.Settings Color.Cyan True "runT"
  ec <- runExceptT (Config.readConfig :: ExceptT Error.E IO Config.Config)
  case ec of
    Left e -> do
      let dlc = Log.defaultConfig
      Log.critical dlc settings "Error config read while run the transfomer:"
      Log.critical dlc settings $ show e
    Right c -> if Config.forks c 
      then forM_ (zip [1,2..] (configToStates c)) $ \(i, s) -> do
        threadDelay (i*1000000)
        forkIO (action s)
      else action (configToState c)      
      where
      action s0 = do
        let cl = Config.log c
        ea <- runExceptT $ runStateT (getTransformer m) s0
        case ea of
          Left e -> do
            Log.error cl settings "Application error: "
            Log.error cl settings $ show e
          Right a -> do
            Log.info cl settings "Result: "
            Log.info cl settings $ show . fst $ a