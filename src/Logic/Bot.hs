{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Logic.Bot where

import Common.Functions (template)
import Control.Monad.State.Lazy (when)
import Interface.Class (IBot, MTrans)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MLog.Exports as Log
import qualified Interface.Messenger.IBot as IBot
import qualified Logic.Logic as Logic
import qualified System.Console.ANSI as Color (Color (..))
import Prelude hiding (init)
import Control.Monad (forM_)

-- | Run bot application
application :: (MTrans m, IBot pointer init _update) => pointer -> m ()
application pointer = do
  Log.setSettings Color.Blue True "application"
  uidFromFile <- Cache.getUpdateIdFromFile
  init <- IBot.getInit pointer
  if uidFromFile
    then do
      uid <- Cache.getUpdateId
      Log.infoM $ template "Received updateId from file: {0}" [show uid]
      longPolling pointer (IBot.setUpdateId init uid) -- updateId from the request, overwrite the one from the file
    else do
      longPolling pointer init

-- | Long polling loop
longPolling :: (MTrans m, IBot pointer init update) => pointer -> init -> m ()
longPolling pointer init = do
  Log.setSettings Color.Cyan True "longPolling"
  (updates, newInit) <- IBot.getUpdates init
  uidFromFile <- Cache.getUpdateIdFromFile
  when uidFromFile do
    uid <- Cache.getUpdateId
    let newuid = IBot.getUpdateId newInit
    Cache.setUpdateId newuid
    Log.infoM $ template "Update updateId in file from {0} to {1}" [show uid, show newuid]
    Cache.writeCache
  calcSendMesages updates
  longPolling pointer newInit

-- | Response to all users
calcSendMesages :: (MTrans m, IBot _pointer _init update) => [update] -> m ()
calcSendMesages = mapM_ $ \update -> do
  list <- Logic.answer update
  Log.infoM "Update config in file..."
  Cache.writeCache
  forM_ list $ uncurry IBot.sendMessage