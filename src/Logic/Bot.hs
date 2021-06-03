{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Logic.Bot where

import Common.Functions (template)
import Interface.Class (IBot, MTrans)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MLog.Exports as Log
import qualified Messenger.Bot.Class as Bot
import qualified Logic.Logic as Logic
import qualified System.Console.ANSI as Color (Color (..))
import Prelude hiding (init)
import Control.Monad ( replicateM_ )

-- | Run bot application
application :: (MTrans m, IBot pointer init _update) => pointer -> m ()
application pointer = do
  Log.setSettings Color.Blue True "application"
  init <- Bot.getInit pointer
  longPolling pointer init
  

-- | Long polling loop
longPolling :: (MTrans m, IBot pointer init update) => pointer -> init -> m ()
longPolling pointer init = do
  Log.setSettings Color.Cyan True "longPolling"
  (updates, newInit) <- Bot.getUpdates init
  calcSendMessages updates
  writeCache newInit
  longPolling pointer newInit

-- | Response to all users
calcSendMessages :: (MTrans m, IBot _pointer _init update) => [update] -> m ()
calcSendMessages = mapM_ $ \update -> do
  (newUpdate, btns, repeatNumber) <- Logic.evalAnswer update
  replicateM_ repeatNumber $ Bot.sendMessage newUpdate btns

writeCache :: (MTrans m, IBot _pointer init _update) => init -> m ()
writeCache init = do
  mUpdateId <- Cache.getmUpdateId
  let newmUpdateId = Bot.getmUpdateId init
  Cache.setmUpdateId newmUpdateId
  Log.infoM $ template "Update updateId in file from {0} to {1}" [show mUpdateId, show newmUpdateId]
  Log.infoM "Update config in file..."
  Cache.writeCache