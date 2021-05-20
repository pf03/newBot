module Interface.MLog.Functions where

import qualified Common.Color as Color
import Common.Misc (Convert (convert), putStrLnT, template)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (encode)
import Data.Aeson.Types (ToJSON)
import qualified Data.ByteString as B
import Data.Char (toUpper)
import Interface.MLog.Class (MLog (..))
import Interface.MLog.Types
  ( ColorScheme,
    Config (..),
    Level (..),
    Settings (Settings, funcName),
  )
import System.Console.ANSI (Color (Black, Blue, Green, Magenta, Red, Yellow))
import Prelude hiding (error)

-----------------------------MLog----------------------------------------------
setColorScheme :: MLog m => ColorScheme -> m ()
setColorScheme newcs = do
  Settings _ le fn <- getSettings
  setSettings newcs le fn

getConfigSettings :: MLog m => m (Config, Settings)
getConfigSettings = do
  config <- getConfig
  settings <- getSettings
  return (config, settings)

resetSettings :: MLog m => m ()
resetSettings = do
  let Settings cs e fn = defaultSettings
  setSettings cs e fn

defaultSettings :: Settings
defaultSettings = Settings Black True ""

defaultConfig :: Config
defaultConfig = Config {colorEnable = False, terminalEnable = True, fileEnable = False, minLevel = 0}

logM :: (MLog m, Show a) => m a -> m a
logM m = do
  a <- m
  debugM a
  return a

-- * An exception has been made for debug information - it can be of any type Show a, not just a String
debugM :: (MLog m, Show a) => a -> m ()
debugM a = messageM Debug (show a)

infoM :: MLog m => String -> m ()
infoM = messageM Info

infoCM :: MLog m => ColorScheme -> String -> m ()
infoCM cs s = do
  setColorScheme cs
  infoM s

warnM :: MLog m => String -> m ()
warnM = messageM Warn

errorM :: MLog m => String -> m ()
errorM = messageM Error

criticalM :: MLog m => String -> m ()
criticalM = messageM Critical

messageM :: MLog m => Level -> String -> m ()
messageM level s = do
  (config, settings) <- getConfigSettings
  message config settings level s

-- Additional functions for debug
getfname :: MLog m => m String
getfname = funcName <$> getSettings

send :: MLog m => m ()
send = do
  fname <- getfname
  infoM $ template "Query {0} sent......" [fname]

receive :: MLog m => m ()
receive = do
  fname <- getfname
  infoM $ template "Response {0} received......" [fname]

receiveData :: (MLog m, Show a) => String -> a -> m ()
receiveData dataName dataValue = do
  fname <- getfname
  infoM $ template "Data {1} received in {0} response......" [fname, dataName]
  debugM dataValue

-----------------------------MonadIO-------------------------------------------
debug :: (MonadIO m, Show a) => Config -> Settings -> a -> m ()
debug lc ls a = messageIO lc ls Debug (show a)

info :: MonadIO m => Config -> Settings -> String -> m ()
info lc ls = messageIO lc ls Info

warn :: MonadIO m => Config -> Settings -> String -> m ()
warn lc ls = messageIO lc ls Warn

error :: MonadIO m => Config -> Settings -> String -> m ()
error lc ls = messageIO lc ls Error

critical :: MonadIO m => Config -> Settings -> String -> m ()
critical lc ls = messageIO lc ls Critical

-----------------------------Default implementation----------------------------
-- The default implementation of the MLog typeclass for the IO monad.
-- In pure code, for example for testing, you can replace this implementation with another one,
-- for example based on writerT, or empty return () implementation
-- Info can be shown in different color schemes, and for other levels the color corresponds to the level
messageIO :: MonadIO m => Config -> Settings -> Level -> String -> m ()
messageIO (Config ecolor eterminal efile ml) (Settings cs en _) level text = do
  if level < toEnum ml || not en
    then return ()
    else do
      when (ecolor && eterminal) $ do
        if level == Info
          then Color.setSchemeT cs
          else Color.setColorT $ getColor level
      when eterminal $ putStrLnT logText
      when efile $ file logText
      when (ecolor && eterminal) Color.resetColorSchemeT
  where
    logText :: String
    logText = map toUpper (show level) <> " " <> text

    getColor :: Level -> Color
    getColor Debug = Green
    getColor Info = Blue -- here you can use different color schemes for the convenience of displaying information
    getColor Warn = Magenta
    getColor Error = Yellow
    getColor Critical = Red

    file :: (MonadIO m, ToJSON a) => a -> m ()
    file str = do
      liftIO $ B.appendFile "log.txt" $ convert . encode $ str
      liftIO $ B.appendFile "log.txt" $ convert ("\n" :: String)