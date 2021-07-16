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
runApplication :: (MTrans m, IBot pointer init _update) => pointer -> m ()
runApplication pointer = do
  Log.setSettings Color.Blue True "application"
  init <- Bot.getInit pointer
  longPollingLoop pointer init

-- | Long polling loop
longPollingLoop :: (MTrans m, IBot pointer init update) => pointer -> init -> m ()
longPollingLoop pointer init = do
  Log.setSettings Color.Cyan True "longPolling"
  (updates, newInit) <- Bot.getUpdates init
  handleUpdates updates
  writeCache newInit
  longPollingLoop pointer newInit

-- | Response to all users
handleUpdates :: (MTrans m, IBot _pointer _init update) => [update] -> m ()
handleUpdates = mapM_ $ \update -> do
  (newUpdate, btns, repeatNumber) <- Logic.evalAnswer update
  replicateM_ repeatNumber $ Bot.sendMessage newUpdate btns

writeCache :: (MTrans m, IBot _pointer init _update) => init -> m ()
writeCache init = do
  mUpdateId <- Cache.getMUpdateId
  let newMUpdateId = Bot.getMUpdateId init
  Cache.setMUpdateId newMUpdateId
  Log.writeInfoM $ template "Update updateId in file from {0} to {1}" [show mUpdateId, show newMUpdateId]
  Log.writeInfoM "Update config in file..."
  Cache.writeCache