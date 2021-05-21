module Transformer.Functions where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State.Lazy (runStateT)
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.Config.Exports as Config
import qualified System.Console.ANSI as Color
import Transformer.Internal as Internal (State (configLog), getApp, readS)
import Transformer.Types ( Transformer(getTransformer) )

run :: Show a => Transformer a -> IO ()
run m = do
  let settings = Log.Settings Color.Cyan True "runT"
  es <- runExceptT (readS :: ExceptT Error.E IO State)
  case es of
    Left e -> do
      let dlc = Log.defaultConfig
      Log.critical dlc settings "Error config read while run the transfomer:"
      Log.critical dlc settings $ show e
    Right s -> do
      let cl = configLog s
      ea <- runExceptT $ runStateT (getTransformer m) s
      case ea of
        Left e -> do
          Log.error cl settings "Application error: "
          Log.error cl settings $ show e
        Right a -> do
          Log.info cl settings "Result: "
          Log.info cl settings $ show . fst $ a

getApp :: Transformer Config.App
getApp = Internal.getApp