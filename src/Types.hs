{-# LANGUAGE DeriveGeneric #-}

--importPriority = 100
module Types where
import qualified Data.Map.Internal as M
import Control.Exception
--mtl
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Trans.Except


import GHC.Generics hiding (S)
import Data.Aeson
import Data.Aeson.Types
import System.Console.ANSI

--Этот модуль содержит все типы в проекте, исключая специфические для каждого из приложений VK, Telegram
--Специфические нужно разместить в VK.Types и Telegram.Types

--------------------------------------Main--------------------------------------------------------
type TimeOut = Int  --таймаут для long polling

--------------------------------------App----------------------------------------------------------
data App = VK | Telegram deriving (Show, Generic)
instance FromJSON App
instance ToJSON App

--------------------------------------Error--------------------------------------------------------
data E = ParseError String | QueryError String | ConfigError String 
-- это надо убрать, раз мы пользуемся мондами Parser - Except - ExceptT
type ES = Either String
type EE = Either E

----------------------------------------Transformer------------------------------------------------
--основной трансформер, контейнер с одним параметром
type T = StateT S (ExceptT E IO)


-----------------------------------------Logic------------------------------------------------------
--1- перезаписать конфиг (если Nothing, то оставляем как есть),  2 - что ответить, 3 - какие посылать кнопки, 4 - сколько повторов???
type Action = (Maybe ConfigApp, Message, [Label])
--1 - Maybe repeatNumber
type Action1 = (Maybe Int, Message, [Label])

------------------------------------Config---------------------------------------------------------

--в случае разветвления логики сделать отдельный конфиг для каждого приложения
--состояние приложения, используется в трансформере, Reader
data S = S {
    app :: App,
    configApp :: ConfigApp,
    configText :: ConfigText,
    configLog :: ConfigLog,
    logSettings :: LogSettings
} deriving (Show, Generic)

data Config = Config {
    _app :: App,
    _apps :: [ConfigApp],
    _text :: ConfigText,
    _log :: ConfigLog
} deriving (Show, Generic)

data ConfigApp = ConfigApp{
    name:: String,
    host :: Host,
    token :: Token,
    updateId :: UpdateId,
    updateIdFromFile :: Bool,
    repeatNumber :: M.Map ChatId Int,
    groupId :: Int,
    version :: String --API version

} deriving (Show, Generic)

data ConfigText = ConfigText{
    help :: String, 
    repeat :: String, 
    unknown :: String, 
    button :: String
} deriving (Show, Generic)

data ConfigLog = ConfigLog{
    color :: Bool,
    terminal :: Bool,
    file :: Bool,
    level :: Int  --уровень включения логов
} deriving (Show, Generic)

instance ToJSON Config where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 1 } 

instance FromJSON Config where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 1 }

instance FromJSON ConfigText
instance ToJSON ConfigText
instance FromJSON ConfigApp
instance ToJSON ConfigApp
instance FromJSON ConfigLog
instance ToJSON ConfigLog

--------------------------------Parse && Logic----------------------------------------------------------
type Token = String
type Host = String
type Path = String
type Key = String -- для удобства парсинга
type UserId = Int
type ChatId = Int
type UpdateId = Int
type IntId = Int --более общее, чтобы не плодить сущности
type StrId = String
type FileId = String
type Message = String  --текстовое сообщение
data Command = Help | Repeat | Start | Unknown String| Button Int deriving (Show, Eq)  --возможные команды боту
type Caption = String  --описание картинки или видео
type Label = String  --подписи на кнопках
type Url = String
type ItemName = String
type UserName = String
type StateChanged = Bool


--Parse
type OResultItem = Object
type OMessageItem = Object

--Log
data LogLevel =  Debug | Data | Info | Warning | Error  deriving (Eq, Enum, Ord)
type ColorScheme = Color
type LogSettings = (ColorScheme, Bool, String)



