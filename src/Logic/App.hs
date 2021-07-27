{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Logic.App where

import Class (IBot, MTrans)
import Common.Functions (template)
import Control.Monad (replicateM_)
import qualified Interface.Cache.Exports as Cache
import qualified Interface.Log.Exports as Log
import qualified Logic.Logic as Logic
import qualified Messenger.Bot.Class as Bot
import qualified System.Console.ANSI as Color (Color (..))
import Prelude hiding (init)

-- | Run bot application
runApplication :: (MTrans m, IBot pointer) => pointer -> m ()
runApplication pointer = do
  Log.setSettings Color.Blue True "application"
  init <- Bot.getInit pointer
  longPollingLoop pointer init

-- | Long polling loop
longPollingLoop :: (MTrans m, IBot pointer) => pointer -> Bot.InitType pointer -> m ()
longPollingLoop pointer init = do
  Log.setSettings Color.Cyan True "longPolling"
  (updates, newInit) <- Bot.getUpdates pointer init
  handleUpdates pointer updates
  writeCache pointer newInit
  longPollingLoop pointer newInit

-- | Response to all users
handleUpdates :: (MTrans m, IBot pointer) => pointer -> [Bot.UpdateType pointer] -> m ()
handleUpdates pointer = mapM_ $ \update -> do
  (newUpdate, btns, repeatNumber) <- Logic.evalAnswer update
  replicateM_ repeatNumber $ Bot.sendMessage pointer newUpdate btns

writeCache :: (MTrans m, IBot pointer) => pointer -> Bot.InitType pointer -> m ()
writeCache pointer init = do
  mUpdateId <- Cache.getMUpdateId
  let newMUpdateId = Bot.getMUpdateId pointer init
  Cache.setMUpdateId newMUpdateId
  Log.writeInfoM $ template "Update updateId in file from {0} to {1}" [show mUpdateId, show newMUpdateId]
  Log.writeInfoM "Update config in file..."
  Cache.writeCache