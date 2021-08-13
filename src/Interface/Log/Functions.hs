module Interface.Log.Functions where

import Common.Convert (Convert (convert))
import Common.Functions (template)
import Control.Monad.State.Lazy (MonadIO (..), gets, modify, when)
import Data.Aeson (encode)
import Data.Aeson.Types (ToJSON)
import qualified Data.ByteString as B
import Data.Char (toUpper)
import qualified Interface.Log.Color as Color
import Interface.Log.Types
import System.Console.ANSI (Color (Black, Blue, Green, Magenta, Red, Yellow))
import Transformer.Types (BotState (stateConfigLog, stateLogSettings), Transformer)

getSettings :: Transformer Settings
getSettings = gets stateLogSettings

setSettings :: ColorScheme -> Enable -> FuncName -> Transformer ()
setSettings colorScheme logEnable funcName = modify $ \state ->
  state {stateLogSettings = Settings colorScheme logEnable funcName}

getConfig :: Transformer Config
getConfig = gets stateConfigLog

setColorScheme :: ColorScheme -> Transformer ()
setColorScheme newColorScheme = do
  Settings _ logEnable funcName <- getSettings
  setSettings newColorScheme logEnable funcName

getConfigSettings :: Transformer (Config, Settings)
getConfigSettings = do
  config <- getConfig
  settings <- getSettings
  return (config, settings)

resetSettings :: Transformer ()
resetSettings = do
  let Settings colorScheme logEnable funcName = defaultSettings
  setSettings colorScheme logEnable funcName

defaultSettings :: Settings
defaultSettings = Settings Black True ""

defaultConfig :: Config
defaultConfig = Config {configColorEnable = False, configTerminalEnable = True, configFileEnable = False, configMinLevel = 0}

withLogM :: (Show a) => Transformer a -> Transformer a
withLogM m = do
  a <- m
  writeDebugM a
  return a

-- * An exception has been made for debug information - it can be of any type Show a, not just a String

writeDebugM :: (Show a) => a -> Transformer ()
writeDebugM a = writeMessageM Debug (show a)

writeInfoM :: String -> Transformer ()
writeInfoM = writeMessageM Info

writeInfoColorM :: ColorScheme -> String -> Transformer ()
writeInfoColorM colorScheme str = do
  setColorScheme colorScheme
  writeInfoM str

writeWarnM :: String -> Transformer ()
writeWarnM = writeMessageM Warn

writeErrorM :: String -> Transformer ()
writeErrorM = writeMessageM Error

writeCriticalM :: String -> Transformer ()
writeCriticalM = writeMessageM Critical

writeMessageM :: Level -> String -> Transformer ()
writeMessageM level str = do
  (config, settings) <- getConfigSettings
  writeMessageIO config settings level str

-- Additional functions for debug
getFuncName :: Transformer String
getFuncName = settingsFuncName <$> getSettings

writeSending :: Transformer ()
writeSending = do
  funcName0 <- getFuncName
  writeInfoM $ template "Query {0} sent......" [funcName0]

writeReceiving :: Transformer ()
writeReceiving = do
  funcName0 <- getFuncName
  writeInfoM $ template "Response {0} received......" [funcName0]

writeReceivingData :: (Show a) => String -> a -> Transformer ()
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
writeMessageIO (Config enableColor enableTerminal enableFile minLevel) (Settings colorScheme logEnable _) level text = do
  if level < toEnum minLevel || not logEnable
    then return ()
    else do
      when (enableColor && enableTerminal) $ do
        if level == Info
          then Color.setSchemeT colorScheme
          else Color.setColorT $ getColor level
      when enableTerminal $ (liftIO . putStrLn) writeLogText
      when enableFile $ writeToFile writeLogText
      when (enableColor && enableTerminal) Color.resetColorSchemeT
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
      liftIO $ B.appendFile "log.txt" $ convert . encode $ str
      liftIO $ B.appendFile "log.txt" $ convert ("\n" :: String)