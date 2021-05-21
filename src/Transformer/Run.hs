{-# LANGUAGE FlexibleInstances #-}

module Transformer.Run where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State.Lazy (StateT (runStateT))
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified System.Console.ANSI as Color
import Transformer.State (S, Transformer)
import qualified Transformer.State as S

runT :: Show a => Transformer a -> IO ()
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