{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Logic.Bot where

-- Our modules
import           Common.Misc
import           Interface.MCache         as Cache
import           Interface.MLog           as Log
import           Interface.MT
import           Interface.Messenger.IBot as Bot
import qualified Logic.Logic              as Logic
import           Interface.MError as Error

-- Other modules
import           Control.Monad.State.Lazy
import           Prelude                  hiding (init)
import qualified System.Console.ANSI      as Color (Color (..))

-- | Run bot application
application :: (MT m, IBot pointer init _update) =>  pointer -> m ()
application pointer = do
    Log.setSettings Color.Blue True "application"
    uidFromFile <- Cache.getUpdateIdFromFile
    init <- Bot.getInit pointer
    if uidFromFile then do
        uid <- Cache.getUpdateId
        Log.infoM $ template "Received updateId from file: {0}" [show uid]
        longPolling pointer (Bot.setUpdateId init uid) -- updateId from the request, overwrite the one from the file
    else do
        longPolling pointer init

-- | Long polling loop
longPolling :: (MT m, IBot pointer init update) => pointer -> init -> m ()
longPolling pointer init = do
    Log.setSettings Color.Cyan True "longPolling"
    (updates, newInit) <-  Bot.getUpdates init
    uidFromFile <- Cache.getUpdateIdFromFile
    when uidFromFile do
        uid <- Cache.getUpdateId
        let newuid = Bot.getUpdateId newInit
        Cache.setUpdateId newuid
        Log.infoM $ template "Update updateId in file from {0} to {1}" [show uid, show newuid]
        Cache.writeCache
    calcSendMesages updates
    longPolling  pointer newInit

-- | Response to all users
calcSendMesages :: (MT m, IBot _pointer _init update) => [update] -> m ()
calcSendMesages = mapM_ $ \update -> do
    (answer, btns) <- Logic.answer update
    Log.infoM "Update config in file..."
    Cache.writeCache
    Bot.sendMessage answer btns