{-# LANGUAGE FlexibleInstances #-}

module T.Transformer where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State.Lazy (StateT (runStateT))
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified System.Console.ANSI as Color
import T.State (S, T)
import qualified T.State as S

runT :: Show a => T a -> IO ()
runT m = do
  let settings = Log.Settings Color.Cyan True "runT"
  es <- runExceptT (S.readS :: ExceptT Error.E IO S)
  case es of
    Left e -> do
      let dlc = Log.defaultConfig
      Log.critical dlc settings "Error config read while run the transfomer:"
      Log.critical dlc settings $ show e
    Right s -> do
      let cl = S.configLog s
      ea <- runExceptT $ runStateT m s
      case ea of
        Left e -> do
          Log.error cl settings "Application error: "
          Log.error cl settings $ show e
        Right a -> do
          Log.info cl settings "Result: "
          Log.info cl settings $ show . fst $ a

-----------------------------Log test------------------------------------------
testLog :: IO ()
testLog = runT $ do
  Log.debugM $ "Debug data value " ++ show [1 .. 10 :: Int] :: T ()
  Log.infoM $ "Info data value " ++ show [1 .. 10 :: Int]
  Log.warnM $ "warnM data value " ++ show [1 .. 10 :: Int]
  Log.errorM $ "Error data value " ++ show [1 .. 10 :: Int]
  Log.criticalM $ "criticalM data value " ++ show [1 .. 10 :: Int]
  Log.infoCM Color.Blue $ "Blue color scheme " ++ klichko
  Log.infoCM Color.Cyan $ "Cyan color scheme " ++ klichko
  Log.infoCM Color.Green $ "Green color scheme " ++ klichko
  Log.infoCM Color.Yellow $ "Yellow color scheme " ++ klichko
  where
    klichko = "Есть очень много по этому поводу точек зрения. Я четко придерживаюсь и четко понимаю, что те проявления, если вы уже так ребром ставите вопрос, что якобы мы"
