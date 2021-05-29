module Logic.Config.Internal where

import Common.Functions (checkUnique, template)
import Control.Monad (forM_, unless, when)
import qualified Data.Map.Internal as M
import Interface.Class (MError)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import Logic.Config.Types (Config (apps, defaultRepeatNumber, log, name))
import Prelude hiding (log)

checkMinLogLevel :: MError m => Config -> m ()
checkMinLogLevel config = do
  let ml = Log.minLevel . log $ config
  let minB = fromEnum (minBound :: Log.Level)
  let maxB = fromEnum (maxBound :: Log.Level)
  when (ml < minB) $
    Error.throw $
      Error.ConfigError $
        template "Min log level shouldn't be less than {0}" [show minB]
  when (ml > maxB) $
    Error.throw $
      Error.ConfigError $
        template "Min log level shouldn't be more than {0}" [show maxB]

maxRepeatNumber :: Int
maxRepeatNumber = 5

checkRepeatNumber :: MError m => Config -> m ()
checkRepeatNumber config = do
  let defaultRepeatNumber0 = defaultRepeatNumber config
  when (defaultRepeatNumber0 < 1) $
    Error.throw $
      Error.ConfigError
        "Default repeat number shouldn't be less than 1"
  when (defaultRepeatNumber0 > maxRepeatNumber) $
    Error.throw $
      Error.ConfigError $
        template "Default repeat number shouldn't be more than {0}" [show maxRepeatNumber]
  let repeatNumbers = concatMap M.toList (Cache.repeatNumber <$> apps config)
  forM_ repeatNumbers $ \(chatId, repeatNumber) -> do
    when (repeatNumber < 1) $
      Error.throw $
        Error.ConfigError $
          template "Repeat number {0} for user {1} shouldn't be less than 1" 
            [show repeatNumber, show chatId]
    when (repeatNumber > maxRepeatNumber) $
      Error.throw $
        Error.ConfigError $
          template "Repeat number {0} for user {1} shouldn't be more than {2}" 
            [show repeatNumber, show chatId, show maxRepeatNumber]

checkUniqueNames :: MError m => Config -> m ()
checkUniqueNames config = do
  unless (checkUnique $ map Cache.name $ apps config) $
    Error.throw $
      Error.ConfigError "Fields apps.name in config.json must be unique"

checkExistAndSingleName :: MError m => Config -> m ()
checkExistAndSingleName config = do
  let name0 = name config
  let names = filter (== name0) $ map Cache.name $ apps config
  when (length names /= 1) $
    Error.throw $
      Error.ConfigError "Field `name` in config.json must exist and be single in list `apps.name`"