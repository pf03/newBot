module Transformer.Functions where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_)
import Control.Monad.State.Lazy (MonadIO (liftIO))
import qualified Logic.Config.Exports as Config
import Transformer.Instances (configToState, configToStates)
import Transformer.Internal (runConfig, runExceptT_, showValue)
import Transformer.Types (Transformer)

run :: Show a => Transformer a -> IO ()
run m = runExceptT_ $ do
  config <- runConfig
  if Config.forks config
    then do
      let states = configToStates config
      liftIO $
        forConcurrently_ (zip [1, 2 ..] states) $ \(i, state) -> do
          threadDelay (i * 1000000)
          runExceptT_ $ showValue config state m
    else do
      let state = configToState config
      showValue config state m