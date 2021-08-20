module Interface.Log.Functions where

import Common.Convert (Convert (convert))
import Common.Functions (template)
import Control.Exception (IOException)
import Control.Monad.State.Lazy (MonadIO (..), gets, modify, when)
import Data.Aeson (encode)
import Data.Aeson.Types (ToJSON)
import qualified Data.ByteString as B
import Data.Char (toUpper)
import qualified Interface.Error.Exports as Error
import qualified Interface.Log.Color as Color
import Interface.Log.Types
  ( ColorScheme (..),
    Config (..),
    Level (..),
    Settings (Settings, settingsFuncName),
  )
import System.Console.ANSI (Color (Blue, Green, Magenta, Red, Yellow))
import Transformer.Types (BotState (stateConfigLog, stateLogSettings), BotStateIO)

getSettings :: BotStateIO Settings
getSettings = gets stateLogSettings

setSettings :: ColorScheme -> Bool -> String -> BotStateIO ()
setSettings colorScheme logEnabled funcName = modify $ \state ->
  state {stateLogSettings = Settings colorScheme logEnabled funcName}

getConfig :: BotStateIO Config
getConfig = gets stateConfigLog

setColorScheme :: ColorScheme -> BotStateIO ()
setColorScheme newColorScheme = do
  Settings _ logEnabled funcName <- getSettings
  setSettings newColorScheme logEnabled funcName

getConfigSettings :: BotStateIO (Config, Settings)
getConfigSettings = do
  config <- getConfig
  settings <- getSettings
  return (config, settings)

defaultSettings :: Settings
defaultSettings = Settings BlackScheme True ""

defaultConfig :: Config
defaultConfig = Config {configColorEnabled = False, configTerminalEnabled = True, configFileEnabled = False, configMinLevel = 0}

-- * An exception has been made for debug information - it can be of any type Show a, not just a String

writeDebugM :: (Show a) => a -> BotStateIO ()
writeDebugM a = writeMessageM Debug (show a)

writeInfoM :: String -> BotStateIO ()
writeInfoM = writeMessageM Info

writeInfoColorM :: ColorScheme -> String -> BotStateIO ()
writeInfoColorM colorScheme str = do
  setColorScheme colorScheme
  writeInfoM str

writeWarnM :: String -> BotStateIO ()
writeWarnM = writeMessageM Warn

writeErrorM :: String -> BotStateIO ()
writeErrorM = writeMessageM Error

writeCriticalM :: String -> BotStateIO ()
writeCriticalM = writeMessageM Critical

writeMessageM :: Level -> String -> BotStateIO ()
writeMessageM level str = do
  (config, settings) <- getConfigSettings
  writeMessageIO config settings level str

-- Additional functions for debug
getFuncName :: BotStateIO String
getFuncName = settingsFuncName <$> getSettings

writeSending :: BotStateIO ()
writeSending = do
  funcName0 <- getFuncName
  writeInfoM $ template "Query {0} sent......" [funcName0]

writeReceiving :: BotStateIO ()
writeReceiving = do
  funcName0 <- getFuncName
  writeInfoM $ template "Response {0} received......" [funcName0]

writeReceivingData :: (Show a) => String -> a -> BotStateIO ()
writeReceivingData dataName dataValue = do
  funcName0 <- getFuncName
  writeInfoM $ template "Data {1} received in {0} response......" [funcName0, dataName]
  writeDebugM dataValue

-----------------------------MonadIO-------------------------------------------
writeDebug :: (MonadIO m, Show a) => Config -> Settings -> a -> m ()
writeDebug logConfig logSettings a = writeMessageIO logConfig logSettings Debug (show a)

writeInfo :: MonadIO m => Config -> Settings -> String -> m ()
writeInfo logConfig logSettings = writeMessageIO logConfig logSettings Info

writeWarn :: MonadIO m => Config -> Settings -> String -> m ()
writeWarn logConfig logSettings = writeMessageIO logConfig logSettings Warn

writeError :: MonadIO m => Config -> Settings -> String -> m ()
writeError logConfig logSettings = writeMessageIO logConfig logSettings Error

writeCritical :: MonadIO m => Config -> Settings -> String -> m ()
writeCritical logConfig logSettings = writeMessageIO logConfig logSettings Critical

-- Info can be shown in different color schemes, and for other levels the color corresponds to the level
writeMessageIO :: MonadIO m => Config -> Settings -> Level -> String -> m ()
writeMessageIO (Config colorEnabled terminalEnabled fileEnabled minLevel) (Settings colorScheme logEnabled _) level text = do
  if level < toEnum minLevel || not logEnabled
    then return ()
    else do
      when (colorEnabled && terminalEnabled) $ do
        if level == Info
          then Color.setScheme colorScheme
          else Color.setColor $ getColor level
      when terminalEnabled $ (liftIO . putStrLn) writeLogText
      when fileEnabled $ writeToFile writeLogText
      when (colorEnabled && terminalEnabled) Color.resetColorScheme
  where
    writeLogText :: String
    writeLogText = map toUpper (show level) <> " " <> text

    getColor :: Level -> Color
    getColor Debug = Green
    getColor Info = Blue -- here you can use different color schemes for the convenience of displaying information
    getColor Warn = Magenta
    getColor Error = Yellow
    getColor Critical = Red

    writeToFile :: (MonadIO m, ToJSON a) => a -> m ()
    writeToFile str = do
      let message = convert (encode str) <> convert ("\n" :: String)
      B.appendFile "log.txt" message `Error.catchEIO` handler
      where
        handler :: IOException -> Error.Error
        handler _ = Error.IOError "Error writing the log to the log.txt file"