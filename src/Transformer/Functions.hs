module Transformer.Functions where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State.Lazy (runStateT)
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.Config.Exports as Config
import qualified System.Console.ANSI as Color
--import Transformer.Internal as Internal (State (configLog), configToStates)
import Transformer.Types --( Transformer(getTransformer) )

runConfig :: ExceptT Error.E IO Config.Config -> IO (Maybe Config.Config)
runConfig m = do
  let settings = Log.Settings Color.Cyan True "runT"
  ec <- runExceptT m
  case ec of
    Left e -> do
      let dlc = Log.defaultConfig
      Log.critical dlc settings "Error config read while run the transfomer:"
      Log.critical dlc settings $ show e
      return Nothing
    Right c -> return $ Just c

run :: Show a => Transformer a -> State ->  IO ()
run m s = do
  let settings = Log.Settings Color.Cyan True "runT"
  let cl = configLog s
  ea <- runExceptT $ runStateT (getTransformer m) s
  case ea of
    Left e -> do
      Log.error cl settings "Application error: "
      Log.error cl settings $ show e
    Right a -> do
      Log.info cl settings "Result: "
      Log.info cl settings $ show . fst $ a




-- getApp :: Transformer Config.App
-- getApp = Internal.getApp

-- configToStates :: Config.Config -> [State]
-- configToStates config = configToStates