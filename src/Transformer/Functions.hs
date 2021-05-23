module Transformer.Functions where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State.Lazy (runStateT)
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.Config.Exports as Config
import qualified System.Console.ANSI as Color
--import Transformer.Internal as Internal (State (configLog), configToStates)
import Transformer.Types --( Transformer(getTransformer) )
import Control.Concurrent
import Control.Monad (forM_)

-- runConfig :: ExceptT Error.E IO Config.Config -> IO (Maybe Config.Config)
-- runConfig m = do
--   let settings = Log.Settings Color.Cyan True "runT"
--   ec <- runExceptT m
--   case ec of
--     Left e -> do
--       let dlc = Log.defaultConfig
--       Log.critical dlc settings "Error config read while run the transfomer:"
--       Log.critical dlc settings $ show e
--       return Nothing
--     Right c -> return $ Just c

-- run :: Show a => Transformer a -> State ->  IO ()
-- run m s = do
--   let settings = Log.Settings Color.Cyan True "runT"
--   let cl = configLog s
--   ea <- runExceptT $ runStateT (getTransformer m) s
--   case ea of
--     Left e -> do
--       Log.error cl settings "Application error: "
--       Log.error cl settings $ show e
--     Right a -> do
--       Log.info cl settings "Result: "
--       Log.info cl settings $ show . fst $ a

-- run :: Show a => Transformer a -> IO ()
-- run m = do

-- run :: Show a => Transformer a -> IO ()
-- run m = do
--   let settings = Log.Settings Color.Cyan True "runT"
--   ec <- runExceptT m
--   case ec of
--     Left e -> do
--       let dlc = Log.defaultConfig
--       Log.critical dlc settings "Error config read while run the transfomer:"
--       Log.critical dlc settings $ show e
--     Right c -> do
--       if Config.forks c then runFork c m else runSingle c m

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


-- runSingle :: Show a => Config.Config -> Transformer a -> IO ()
-- runSingle c m = do
--     let s = configToStates c
--     let cl = configLog s
--     ea <- runExceptT $ runStateT (getTransformer m) s
--     case ea of
--       Left e -> do
--         Log.error cl settings "Application error: "
--         Log.error cl settings $ show e
--       Right a -> do
--         Log.info cl settings "Result: "
--         Log.info cl settings $ show . fst $ a

-- runSingle :: Show a => Transformer a -> IO ()
-- runSingle m = do
--   let settings = Log.Settings Color.Cyan True "runT"
--   es <- runExceptT (readState :: ExceptT Error.E IO State)
--   case es of
--     Left e -> do
--       let dlc = Log.defaultConfig
--       Log.critical dlc settings "Error config read while run the transfomer:"
--       Log.critical dlc settings $ show e
--     Right s -> do
--       let cl = configLog s
--       ea <- runExceptT $ runStateT (getTransformer m) s
--       case ea of
--         Left e -> do
--           Log.error cl settings "Application error: "
--           Log.error cl settings $ show e
--         Right a -> do
--           Log.info cl settings "Result: "
--           Log.info cl settings $ show . fst $ a

-- runFork :: Show a => Config.Config -> Transformer a -> IO ()
-- runFork m = do
--   let settings = Log.Settings Color.Cyan True "runT"
--   ess <- runExceptT (readStates :: ExceptT Error.E IO [State])
--   case ess of
--     Left e -> do
--       let dlc = Log.defaultConfig
--       Log.critical dlc settings "Error config read while run the transfomer:"
--       Log.critical dlc settings $ show e
--     Right ss -> do
--       forM_ ss $ \s -> forkIO $ do
--         let cl = configLog s
--         ea <- runExceptT $ runStateT (getTransformer m) s
--         case ea of
--           Left e -> do
--             Log.error cl settings "Application error: "
--             Log.error cl settings $ show e
--           Right a -> do
--             Log.info cl settings "Result: "
--             Log.info cl settings $ show . fst $ a




-- getApp :: Transformer Config.App
-- getApp = Internal.getApp

-- configToStates :: Config.Config -> [State]
-- configToStates config = configToStates