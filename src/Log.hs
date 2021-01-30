
module Log where 
--importPriority = 25

import Types
import Colors
import Common
import System.Console.ANSI
import Control.Monad
import Control.Monad.State.Lazy
import App
import Class
import Control.Applicative
import Data.Maybe
import Data.Aeson.Types hiding (Error)
import Data.Aeson hiding (Error)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

--упорядочить типы логов и цветов
--тип T нам нужен для доступа к конфигу

logSendT :: MonadLog m => m ()
logSendT = do
    fname <- getfnameT 
    logTextT Info $ template "Отправляем запрос {0}.................." [fname]

logReceiveT :: MonadLog m => m ()
logReceiveT = do 
    fname <- getfnameT 
    logTextT Info $ template "Получили ответ {0}.................." [fname]

logReceiveDataT :: (MonadLog m, Show a) => String -> a -> m ()
logReceiveDataT dataName dataValue = do
    fname <- getfnameT 
    logTextT Info $ template "Получили {1} в запросе {0}.................." [fname, dataName]
    logDataT Data dataValue

logReceiveConvertDataT :: (MonadLog m, Show a, ToJSON a) => String -> a -> m ()
logReceiveConvertDataT dataName dataValue = do
    fname <- getfnameT 
    logTextT Info $ template "Получили {1} в запросе {0}.................." [fname, dataName]
    logConvertDataT Data dataValue

logErrorT :: (MonadLog m, Show e) => e -> m ()
logErrorT error = do
    (config, settings) <- getConfigSettings
    liftIO $ logError config settings error

--всего лишь добавляем название функции для удобства отладки
logFuncT :: MonadLog m => LogLevel -> String -> m ()
logFuncT  logLevel text = do
    fname <- getfnameT 
    logTextT logLevel $ template "Функция {0}:" [fname]

logColorTextT :: MonadLog m => ColorScheme -> LogLevel -> String -> m ()
logColorTextT colorScheme logLevel text = do
    setLogSettings (colorScheme, True, "")
    logTextT logLevel text

logTextT :: MonadLog m => LogLevel -> String -> m ()
logTextT logLevel text = do
    (config, msettings) <- getConfigSettings
    liftIO $ logText config msettings logLevel text

logDataT :: (MonadLog m, Show a) => LogLevel -> a -> m ()
logDataT logLevel dataValue = do
    (config, settings) <- getConfigSettings
    liftIO $ logData config settings logLevel dataValue

logConvertDataT :: (MonadLog m, Show a, ToJSON a) => LogLevel -> a -> m ()
logConvertDataT logLevel dataValue = do
    (config, settings) <- getConfigSettings
    liftIO $ logConvertData config settings logLevel dataValue

getfnameT :: MonadLog m => m String 
getfnameT = getfname <$> getLogSettings

getConfigSettings :: MonadLog m => m (ConfigLog, LogSettings) 
getConfigSettings = do
    config <- getLogConfig 
    settings <- getLogSettings
    return (config, settings)

resetLogSettings :: MonadLog m => m ()
resetLogSettings = setLogSettings defaultLogSettings


--более низкоуровневые функции, если нету доступа к трансформеру, например в runT
--Текст идет с цветовой схемой
logError :: (Show a) => ConfigLog -> LogSettings -> a -> IO()
logError config settings error = do
    let fname = getfname settings
    logText config settings Error $ template "Получили ошибку в функции {0}!" [fname]
    logData config settings Error error


logText :: ConfigLog -> LogSettings -> LogLevel -> String -> IO ()
logText (ConfigLog color terminal file configLevel) (colorScheme, enable , _ ) logLevel text = do
    if not $ logLevel >= toEnum configLevel && enable then return () else do
        when (color && terminal) $ setSchemeT colorScheme
        when terminal $ putStrLnT text
        when file $ logFile text
        when (color && terminal) resetColorSchemeT 

--Данные с цветом, зависяцим от LogLevel--logData не зависит от настроек цвета, только logText зависит
logData :: (Show a) => ConfigLog -> LogSettings -> LogLevel -> a -> IO ()
logData (ConfigLog color terminal file configLevel) (colorScheme, enable , _ ) logLevel dataValue = do
    if not $ logLevel >= toEnum configLevel && enable then return () else do
        when (color && terminal) $ setColorT $ getColor logLevel
        when terminal $ printT dataValue
        when file $ logFile $ show dataValue
        when (color && terminal) resetColorSchemeT 
-------------эти две функции объединить в одну----------------------------------------------------------------
logConvertData :: (ToJSON a, Show a) => ConfigLog -> LogSettings -> LogLevel -> a -> IO ()
logConvertData (ConfigLog color terminal file configLevel) (colorScheme, enable , _ ) logLevel dataValue = do
    if not $ logLevel >= toEnum configLevel && enable then return () else do
        when (color && terminal) $ setColorT $ getColor logLevel
        when terminal $ printT dataValue
        --when file $ logFile $ show dataValue
        when file $ logFile dataValue
        when (color && terminal) resetColorSchemeT 

defaultLogSettings :: LogSettings
defaultLogSettings = (Black, True, "")

defaultLogConfig :: ConfigLog
defaultLogConfig = ConfigLog {color = False, terminal = True, file = False, level = 0}

logFile :: ToJSON a => a -> IO()
logFile str = do 
    B.appendFile "log.txt" $ convert . encode $ str --строгая версия
    B.appendFile "log.txt" $ convert ("\n" :: String)  --строгая версия

clearLogFile :: IO()
clearLogFile = B.writeFile "log.txt" $ convert ("" :: String)

getColor :: LogLevel -> Color
getColor  Data = Green
getColor  Info = Blue
getColor  Error = Red
getColor  Warning = Yellow
getColor  Debug = Magenta 

getfname :: LogSettings -> String 
-- getfname Nothing = "?"
getfname (_ , _,fn) = fn
