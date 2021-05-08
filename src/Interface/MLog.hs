{-# LANGUAGE DeriveGeneric #-}
module Interface.MLog 
-- (sendT,
-- receiveT,
-- receiveDataT,
-- receiveConvertDataT,
-- errorT,
-- funcT,
-- colorTextT,
-- textT,
-- dataT,
-- convertDataT,
-- resetSettings,
-- error,
-- text,
-- ldata,
-- convertData,
-- defaultSettings,
-- defaultConfig,
-- lfile,
-- clearFile) 
where 
--importPriority = 25

-- Our molules
import qualified Common.Color as Color
import Common.Misc

-- Other modules
import System.Console.ANSI
import Control.Monad
import Control.Monad.State.Lazy
-- import App
import Control.Applicative
-- import Data.Maybe
import Data.Aeson.Types hiding (Error)
import Data.Aeson hiding (Error)
import Prelude hiding (error)
import GHC.Generics

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

-----------------------------Types---------------------------------------------
data LogLevel =  Debug | Data | Info | Warning | Error  deriving (Eq, Enum, Ord)
type ColorScheme = Color
type LogSettings = (ColorScheme, Bool, String)

data ConfigLog = ConfigLog{
    colorEnable :: Bool,
    terminalEnable :: Bool,
    fileEnable :: Bool,
    minLevel :: Int  --уровень включения логов
} deriving (Show, Generic)

instance FromJSON ConfigLog
instance ToJSON ConfigLog


class MonadIO m => MLog m where
  getSettings :: m LogSettings
  setSettings :: LogSettings -> m()
  --resetLogSettings :: m ()
  --resetLogSettings = setLogSettings defaultLogSettings  перенес в hs как свободную функцию
  getConfig :: m ConfigLog

sendT :: MLog m => m ()
sendT = do
    fname <- getfnameT 
    textT Info $ template "Отправляем запрос {0}.................." [fname]

receiveT :: MLog m => m ()
receiveT = do 
    fname <- getfnameT 
    textT Info $ template "Получили ответ {0}.................." [fname]

receiveDataT :: (MLog m, Show a) => String -> a -> m ()
receiveDataT dataName dataValue = do
    fname <- getfnameT 
    textT Info $ template "Получили {1} в запросе {0}.................." [fname, dataName]
    dataT Data dataValue

receiveConvertDataT :: (MLog m, Show a, ToJSON a) => String -> a -> m ()
receiveConvertDataT dataName dataValue = do
    fname <- getfnameT 
    textT Info $ template "Получили {1} в запросе {0}.................." [fname, dataName]
    convertDataT Data dataValue

errorT :: (MLog m, Show e) => e -> m ()
errorT error = do
    (config, settings) <- getConfigSettings
    liftIO $ Interface.MLog.error config settings error

--всего лишь добавляем название функции для удобства отладки
funcT :: MLog m => LogLevel -> String -> m ()
funcT level text = do
    fname <- getfnameT 
    textT level $ template "Функция {0}:" [fname]

colorTextT :: MLog m => ColorScheme -> LogLevel -> String -> m ()
colorTextT colorScheme level text = do
    setSettings (colorScheme, True, "")
    textT level text

textT :: MLog m => LogLevel -> String -> m ()
textT level text = do
    (config, msettings) <- getConfigSettings
    liftIO $ Interface.MLog.text config msettings level text

dataT :: (MLog m, Show a) => LogLevel -> a -> m ()
dataT level dataValue = do
    (config, settings) <- getConfigSettings
    liftIO $ ldata config settings level dataValue

convertDataT :: (MLog m, Show a, ToJSON a) => LogLevel -> a -> m ()
convertDataT level dataValue = do
    (config, settings) <- getConfigSettings
    liftIO $ convertData config settings level dataValue

getfnameT :: MLog m => m String 
getfnameT = getfname <$> getSettings

getConfigSettings :: MLog m => m (ConfigLog, LogSettings) 
getConfigSettings = do
    config <- getConfig 
    settings <- getSettings
    return (config, settings)

resetSettings :: MLog m => m ()
resetSettings = setSettings defaultSettings


--более низкоуровневые функции, если нету доступа к трансформеру, например в runT
--Текст идет с цветовой схемой
error :: (Show a) => ConfigLog -> LogSettings -> a -> IO()
error config settings error = do
    let fname = getfname settings
    text config settings Error $ template "Получили ошибку в функции {0}!" [fname]
    ldata config settings Error error


text :: ConfigLog -> LogSettings -> LogLevel -> String -> IO ()
text (ConfigLog color terminal file configLevel) (colorScheme, enable , _ ) level text = do
    if not $ level >= toEnum configLevel && enable then return () else do
        when (color && terminal) $ Color.setSchemeT colorScheme
        when terminal $ putStrLnT text
        when file $ Interface.MLog.file text
        when (color && terminal) Color.resetColorSchemeT 

--Данные с цветом, зависяцим от LogLevel--logData не зависит от настроек цвета, только logText зависит
ldata :: (Show a) => ConfigLog -> LogSettings -> LogLevel -> a -> IO ()
ldata (ConfigLog color terminal file configLevel) (colorScheme, enable , _ ) level dataValue = do
    if not $ level >= toEnum configLevel && enable then return () else do
        when (color && terminal) $ Color.setColorT $ getColor level
        when terminal $ printT dataValue
        when file $ Interface.MLog.file $ show dataValue
        when (color && terminal) Color.resetColorSchemeT 
-------------эти две функции объединить в одну----------------------------------------------------------------
convertData :: (ToJSON a, Show a) => ConfigLog -> LogSettings -> LogLevel -> a -> IO ()
convertData (ConfigLog color terminal file configLevel) (colorScheme, enable , _ ) level dataValue = do
    if not $ level >= toEnum configLevel && enable then return () else do
        when (color && terminal) $ Color.setColorT $ getColor level
        when terminal $ printT dataValue
        --when file $ logFile $ show dataValue
        when file $ Interface.MLog.file dataValue
        when (color && terminal) Color.resetColorSchemeT 

defaultSettings :: LogSettings
defaultSettings = (Black, True, "")

defaultConfig :: ConfigLog
defaultConfig = ConfigLog {colorEnable = False, terminalEnable = True, fileEnable = False, minLevel = 0}

file :: ToJSON a => a -> IO()
file str = do 
    B.appendFile "log.txt" $ convert . encode $ str --строгая версия
    B.appendFile "log.txt" $ convert ("\n" :: String)  --строгая версия

clearFile :: IO()
clearFile = B.writeFile "log.txt" $ convert ("" :: String)

getColor :: LogLevel -> Color
getColor  Data = Green
getColor  Info = Blue
getColor  Error = Red
getColor  Warning = Yellow
getColor  Debug = Magenta 

getfname :: LogSettings -> String 
-- getfname Nothing = "?"
getfname (_ , _,fn) = fn


