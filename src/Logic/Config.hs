{-# LANGUAGE DeriveGeneric #-}

module Logic.Config where

import Common.Misc (template)
import Control.Exception (IOException)
import Control.Monad (forM_, when)
import Data.Aeson
  ( FromJSON (parseJSON),
    Options (fieldLabelModifier),
    ToJSON (toJSON),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Internal as M
import GHC.Generics (Generic)
import Interface.Class (MError, MIOError)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.Parse.Functions as Parse (eDecode)
import System.IO.Error (isDoesNotExistError)

-----------------------------Types---------------------------------------------
data App = Telegram | VK deriving (Show, Generic)

instance ToJSON App

instance FromJSON App

data Config = Config
  { _app :: App,
    _defaultRepeatNumber :: Int,
    _apps :: [Cache.ConfigApp],
    _text :: Cache.ConfigText,
    _log :: Log.Config
  }
  deriving (Show, Generic)

instance ToJSON Config where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = drop 1
        }

instance FromJSON Config where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = drop 1
        }

readConfig :: MIOError m => m Config
readConfig = do
  bs <- L.readFile pathConfig `Error.catchEIO` handler
  c <- Parse.eDecode bs
  checkMinLogLevel c
  checkRepeatNumber c
  return c
  where
    handler :: IOException -> Error.E
    handler e
      | isDoesNotExistError e = Error.ConfigError "Configuration file not found!"
      | otherwise = Error.ConfigError "Error reading configuration file"

pathConfig :: FilePath
pathConfig = "config.json"

-----------------------------Check---------------------------------------------
checkMinLogLevel :: MError m => Config -> m ()
checkMinLogLevel c = do
  let ml = Log.minLevel . _log $ c
  let minB = fromEnum (minBound :: Log.Level)
  let maxB = fromEnum (maxBound :: Log.Level)
  --Error.throw $ ConfigError $ template "{0}={1}" [show $ fromEnum minB, show $ fromEnum maxB]
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
checkRepeatNumber c = do
  let drn = _defaultRepeatNumber c
  when (drn < 1) $
    Error.throw $
      Error.ConfigError
        "Default repeat number shouldn't be less than 1"
  when (drn > maxRepeatNumber) $
    Error.throw $
      Error.ConfigError $
        template "Default repeat number shouldn't be more than {0}" [show maxRepeatNumber]
  let rns = concatMap M.toList (Cache.repeatNumber <$> _apps c)
  -- Error.throw $ ConfigError $ show rns
  forM_ rns $ \(cid, rn) -> do
    when (rn < 1) $
      Error.throw $
        Error.ConfigError $
          template "Repeat number {0} for user {1} shouldn't be less than 1" [show rn, show cid]
    when (rn > maxRepeatNumber) $
      Error.throw $
        Error.ConfigError $
          template "Repeat number {0} for user {1} shouldn't be more than {2}" [show rn, show cid, show maxRepeatNumber]