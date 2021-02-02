{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE FunctionalDependencies #-}

--{-# LANGUAGE TypeApplications #-}
--importPriority = 0
module Main where

import Control.Concurrent

--наши модули
import qualified Telegram.Main as Telegram  --1
import qualified VK.Main as VK --1
import qualified App --60
-- import Error 
-- import Parse --( getObject, getValue, parseChatMessage )
import Config --( readConfig ) --40
-- import API --( querySendMessage, sendRequest, API(SendMessage, GetUpdates) )
import Logic --hiding (Env(..)) --30
import Types hiding (S(..))--100
import qualified VK.Types --(Pointer) как то нужно ограничить, чтобы ничего больше не импортировать --99
import qualified Telegram.Types --(Pointer) как то нужно ограничить, чтобы ничего больше не импортировать --99
import Transformer
import qualified Log
import Common
import Class
import qualified State as S

import Control.Monad.State.Lazy
import System.Console.ANSI
import Data.Maybe
--import GHC.Generics  --зачем это?

main:: IO()
main = do
  exVar <- newMVar False
  forkIO $ runT switchApplication
  exit exVar

switchApplication :: T () 
switchApplication = do
  app <- S.getApp
  case app of
    VK -> application VK.Types.Pointer 
    Telegram -> application Telegram.Types.Pointer

application :: (App.Main pointer init _update) => pointer -> T () 
application pointer = do 
  Log.setSettings (Blue, True, "application") 
  updateIdFromFile <- S.getUpdateIdFromFile
  init <- App.getInit pointer
  if updateIdFromFile
      then do
          updateId <- S.getUpdateId
          Log.funcT Info $ template "Получили updateId из файла: {0}" [show updateId] 
          longPolling pointer (App.setUpdateId init updateId)  --updateId из запроса перезаписываем тем, что из файла
      else do
          longPolling pointer init

longPolling :: (App.Main pointer init update) => pointer -> init -> T () 
longPolling pointer init = do 
  Log.setSettings (Cyan, True, "longPolling")
  (updates, newInit) <-  App.getUpdates init
  updateIdFromFile <- S.getUpdateIdFromFile
  when updateIdFromFile do 
      uid <- S.getUpdateId
      let newuid = App.getUpdateId newInit
      S.setUpdateId newuid
      Log.funcT Info $ template "Обновляем updateId в файле с {0} на {1}" [show uid, show newuid]
      saveST --updateId из файла перезаписываем тем, что из запроса
  calcSendMesages updates
  longPolling pointer newInit

--отвечаем всем пользователям
calcSendMesages :: (App.Main _pointer _init update) => [update] -> T ()
calcSendMesages = mapM_ $ \update -> do 
  (stateChanged, answer, btns) <- toT $ answer update
  --обновляем конфиг
  when stateChanged $ do
      putStrLnT "Обновляем конфиг..."
      saveST
  App.sendMessage answer btns

--вывод на консоль должен быть в порядке очередности
exit :: MVar Bool -> IO()
exit exVar = do 
  threadDelay 2000000
  putStrLn "Введите exit для выхода из приложения"
  line <-getLine
  if line == "exit" then do 
    putStrLn "Выходим из приложения прямо сейчас..."
    --putMVar exVar True
  else do
    putStrLn "Неверная команда"
    exit exVar






  


