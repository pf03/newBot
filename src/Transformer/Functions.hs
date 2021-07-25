module Transformer.Functions where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception
import Control.Monad.State.Lazy (MonadIO (liftIO))
import qualified Interface.Cache.Config.Exports as Config
import qualified Interface.Error.Exports as Error
import Transformer.Internal (runConfig, showValue)
import Transformer.State (getStateFromConfig, getStatesFromConfig)
import Transformer.Types (Transformer)

run :: Show a => Transformer a -> IO ()
run m = handle errorEmptyHandler $ do
  config <- runConfig
  if Config.configForks config
    then do
      let states = getStatesFromConfig config
      liftIO $
        forConcurrently_ (zip [1, 2 ..] states) $ \(i, state) -> do
          threadDelay (i * 1000000)
          showValue config state m
    else do
      let state = getStateFromConfig config
      showValue config state m
  where
    errorEmptyHandler :: Error.Error -> IO ()
    errorEmptyHandler _ = return ()