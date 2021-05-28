{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Logic.Bot where

import Common.Functions (template)
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
  init <- IBot.getInit pointer
  longPolling pointer init
  

-- | Long polling loop
longPolling :: (MTrans m, IBot pointer init update) => pointer -> init -> m ()
longPolling pointer init = do
  Log.setSettings Color.Cyan True "longPolling"
  (updates, newInit) <- IBot.getUpdates init
  calcSendMesages updates
  writeCache newInit
  longPolling pointer newInit

-- | Response to all users
calcSendMesages :: (MTrans m, IBot _pointer _init update) => [update] -> m ()
calcSendMesages = mapM_ $ \update -> do
  list <- Logic.answer update
  forM_ list $ uncurry IBot.sendMessage

writeCache :: (MTrans m, IBot _pointer init _update) => init -> m ()
writeCache init = do
  mUpdateId <- Cache.getmUpdateId
  let newmUpdateId = IBot.getmUpdateId init
  Cache.setmUpdateId newmUpdateId
  Log.infoM $ template "Update updateId in file from {0} to {1}" [show mUpdateId, show newmUpdateId]
  Log.infoM "Update config in file..."
  Cache.writeCache