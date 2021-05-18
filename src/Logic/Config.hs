{-# LANGUAGE DeriveGeneric #-}

module Logic.Config where

import Control.Exception (IOException)
import Data.Aeson
  ( FromJSON (parseJSON),
    Options (fieldLabelModifier),
    ToJSON (toJSON),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import qualified Data.ByteString.Lazy as L
import GHC.Generics (Generic)
import Interface.MCache (ConfigApp, ConfigText)
import qualified Interface.MCache as Cache
import Interface.MError (E (ConfigError), MIOError, MError)
import qualified Interface.MError as Error
import Interface.MLog.Types (LogConfig, LogLevel, minLevel)
import qualified Logic.Parse as Parse (eDecode)
import System.IO.Error (isDoesNotExistError)
import Control.Monad (when, forM_)
import Common.Misc (template)
import qualified Data.Map.Internal as M

-----------------------------Types---------------------------------------------
data App = Telegram | VK deriving (Show, Generic)

instance ToJSON App

instance FromJSON App

data Config = Config
  { _app :: App,
    _defaultRepeatNumber :: Int,
    _apps :: [ConfigApp],
    _text :: ConfigText,
    _log :: LogConfig
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
    handler :: IOException -> E
    handler e
      | isDoesNotExistError e = ConfigError "Configuration file not found!"
      | otherwise = ConfigError "Error reading configuration file"

pathConfig :: FilePath
pathConfig = "config.json"

-----------------------------Check---------------------------------------------
checkMinLogLevel :: MError m => Config -> m ()
checkMinLogLevel c = do
  let ml = minLevel . _log $ c
  let minB = fromEnum (minBound :: LogLevel)
  let maxB = fromEnum(maxBound :: LogLevel)
  --Error.throw $ ConfigError $ template "{0}={1}" [show $ fromEnum minB, show $ fromEnum maxB]
  when (ml < minB) $ Error.throw $ ConfigError $ 
    template "Min log level shouldn't be less than {0}" [show minB]
  when (ml > maxB) $ Error.throw $ ConfigError $ 
    template "Min log level shouldn't be more than {0}" [show maxB]

maxRepeatNumber :: Int
maxRepeatNumber = 5

checkRepeatNumber :: MError m => Config -> m ()
checkRepeatNumber c = do
  let drn = _defaultRepeatNumber c
  when (drn < 1) $ Error.throw $ ConfigError
    "Default repeat number shouldn't be less than 1"
  when (drn > maxRepeatNumber) $ Error.throw $ ConfigError $ 
    template "Default repeat number shouldn't be more than {0}" [show maxRepeatNumber]
  let rns = concatMap M.toList (Cache.repeatNumber <$> _apps c)
  -- Error.throw $ ConfigError $ show rns
  forM_ rns $ \(cid, rn) -> do
      when (rn < 1) $ Error.throw $ ConfigError $
        template "Repeat number {0} for user {1} shouldn't be less than 1" [show rn, show cid]
      when (rn > maxRepeatNumber) $ Error.throw $ ConfigError $ 
        template "Repeat number {0} for user {1} shouldn't be more than {2}" [show rn, show cid, show maxRepeatNumber]