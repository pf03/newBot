module Logic.App where

import Common.Functions (template)
import Control.Monad (replicateM_)
import qualified Interface.Cache.Config.Exports as Config
import qualified Interface.Cache.Exports as Cache
import qualified Interface.Log.Exports as Log
import qualified Logic.Logic as Logic
import qualified Messenger.Bot.Class as Bot
import qualified System.Console.ANSI as Color (Color (..))
import Transformer.Types (BotStateIO)
import Prelude hiding (init)

runApplication :: (Bot.IBot pointer) => pointer -> BotStateIO ()
runApplication pointer = do
  Log.setSettings Color.Blue True "application"
  init <- Bot.getInit pointer
  longPollingLoop pointer init

longPollingLoop :: (Bot.IBot pointer) => pointer -> Bot.InitType pointer -> BotStateIO ()
longPollingLoop pointer init = do
  Log.setSettings Color.Cyan True "longPolling"
  (updates, newInit) <- Bot.getUpdates pointer init
  handleUpdates pointer updates
  writeCache pointer newInit
  longPollingLoop pointer newInit

handleUpdates :: (Bot.IBot pointer) => pointer -> [Bot.UpdateType pointer] -> BotStateIO ()
handleUpdates pointer = mapM_ $ \update -> do
  (newUpdate, btns, repeatNumber) <- Logic.evalAnswer update
  replicateM_ repeatNumber $ Bot.sendMessage pointer newUpdate btns

writeCache :: (Bot.IBot pointer) => pointer -> Bot.InitType pointer -> BotStateIO ()
writeCache pointer init = do
  mUpdateId <- Cache.getMUpdateId
  let newMUpdateId = Bot.getMUpdateId pointer init
  Cache.setMUpdateId newMUpdateId
  Log.writeInfoM $ template "Update updateId in file from {0} to {1}" [show mUpdateId, show newMUpdateId]
  Log.writeInfoM "Update config in file..."
  Config.writeCacheToConfigFile