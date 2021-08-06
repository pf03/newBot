module Interface.Cache.Config.Internal where

import Common.Functions (checkUnique)
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map.Internal as M
import Interface.Cache.Config.Types
  ( Config (configApps, configDefaultRepeatNumber, configLog, configName),
    ConfigApp (appName, appRepeatNumber),
  )
import qualified Interface.Error.Exports as Error
import qualified Interface.Log.Types as Log
import Prelude hiding (log)

checkMinLogLevel :: MonadIO m => Config -> m ()
checkMinLogLevel config = do
  let minLevel = Log.configMinLevel . configLog $ config
  let minB = fromEnum (minBound :: Log.Level)
  let maxB = fromEnum (maxBound :: Log.Level)
  when (minLevel < minB) $ Error.throwConfig "Min log level shouldn't be less than {0}" [show minB]
  when (minLevel > maxB) $ Error.throwConfig "Min log level shouldn't be more than {0}" [show maxB]

maxRepeatNumber :: Int
maxRepeatNumber = 5

checkRepeatNumber :: MonadIO m => Config -> m ()
checkRepeatNumber config = do
  let defaultRepeatNumber0 = configDefaultRepeatNumber config
  when (defaultRepeatNumber0 < 1) $
    Error.throwConfig "Default repeat number shouldn't be less than 1" []
  when (defaultRepeatNumber0 > maxRepeatNumber) $
    Error.throwConfig "Default repeat number shouldn't be more than {0}" [show maxRepeatNumber]
  let repeatNumbers = concatMap M.toList (appRepeatNumber <$> configApps config)
  forM_ repeatNumbers $ \(chatId, repeatNumber) -> do
    when (repeatNumber < 1) $
      Error.throwConfig
        "Repeat number {0} for user {1} shouldn't be less than 1"
        [show repeatNumber, show chatId]
    when (repeatNumber > maxRepeatNumber) $
      Error.throwConfig
        "Repeat number {0} for user {1} shouldn't be more than {2}"
        [show repeatNumber, show chatId, show maxRepeatNumber]

checkUniqueNames :: MonadIO m => Config -> m ()
checkUniqueNames config = do
  unless (checkUnique $ map appName $ configApps config) $
    Error.throwConfig "Fields apps.name in config.json must be unique" []

checkExistAndSingleName :: MonadIO m => Config -> m ()
checkExistAndSingleName config = do
  let name = configName config
  let names = filter (== name) $ map appName $ configApps config
  when (length names /= 1) $
    Error.throwConfig "Field `name` in config.json must exist and be single in list `apps.name`" []