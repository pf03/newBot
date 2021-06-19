module Interface.Log.Functions where

import Common.Convert (Convert (convert))
import Common.Functions (putStrLnT, template)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (encode)
import Data.Aeson.Types (ToJSON)
import qualified Data.ByteString as B
import Data.Char (toUpper)
import Interface.Log.Class (MLog (..))
import qualified Interface.Log.Color as Color
import Interface.Log.Types
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
  Settings _ logEnable funcName0 <- getSettings
  setSettings newcs logEnable funcName0

getConfigSettings :: MLog m => m (Config, Settings)
getConfigSettings = do
  config <- getConfig
  settings <- getSettings
  return (config, settings)

resetSettings :: MLog m => m ()
resetSettings = do
  let Settings colorScheme logEnable funcName0 = defaultSettings
  setSettings colorScheme logEnable funcName0

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
infoCM colorScheme str = do
  setColorScheme colorScheme
  infoM str

warnM :: MLog m => String -> m ()
warnM = messageM Warn

errorM :: MLog m => String -> m ()
errorM = messageM Error

criticalM :: MLog m => String -> m ()
criticalM = messageM Critical

messageM :: MLog m => Level -> String -> m ()
messageM level str = do
  (config, settings) <- getConfigSettings
  message config settings level str

-- Additional functions for debug
getFuncName :: MLog m => m String
getFuncName = funcName <$> getSettings

send :: MLog m => m ()
send = do
  funcName0 <- getFuncName
  infoM $ template "Query {0} sent......" [funcName0]

receive :: MLog m => m ()
receive = do
  funcName0 <- getFuncName
  infoM $ template "Response {0} received......" [funcName0]

receiveData :: (MLog m, Show a) => String -> a -> m ()
receiveData dataName dataValue = do
  funcName0 <- getFuncName
  infoM $ template "Data {1} received in {0} response......" [funcName0, dataName]
  debugM dataValue

-----------------------------MonadIO-------------------------------------------
debug :: (MonadIO m, Show a) => Config -> Settings -> a -> m ()
debug logConfig logSettings a = messageIO logConfig logSettings Debug (show a)

info :: MonadIO m => Config -> Settings -> String -> m ()
info logConfig logSettings = messageIO logConfig logSettings Info

warn :: MonadIO m => Config -> Settings -> String -> m ()
warn logConfig logSettings = messageIO logConfig logSettings Warn

error :: MonadIO m => Config -> Settings -> String -> m ()
error logConfig logSettings = messageIO logConfig logSettings Error

critical :: MonadIO m => Config -> Settings -> String -> m ()
critical logConfig logSettings = messageIO logConfig logSettings Critical

-----------------------------Default implementation----------------------------
-- The default implementation of the MLog typeclass for the IO monad.
-- In pure code, for example for testing, you can replace this implementation with another one,
-- for example based on writerT, or empty return () implementation
-- Info can be shown in different color schemes, and for other levels the color corresponds to the level
messageIO :: MonadIO m => Config -> Settings -> Level -> String -> m ()
messageIO (Config enableColor enableTerminal enableFile minLevel0) (Settings colorScheme logEnable _) level text = do
  if level < toEnum minLevel0 || not logEnable
    then return ()
    else do
      when (enableColor && enableTerminal) $ do
        if level == Info
          then Color.setSchemeT colorScheme
          else Color.setColorT $ getColor level
      when enableTerminal $ putStrLnT logText
      when enableFile $ file logText
      when (enableColor && enableTerminal) Color.resetColorSchemeT
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