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
import Types --100
import qualified VK.Types --(Pointer) как то нужно ограничить, чтобы ничего больше не импортировать --99
import qualified Telegram.Types --(Pointer) как то нужно ограничить, чтобы ничего больше не импортировать --99
import Transformer
import Log
import Common
import Class

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
  app <- gets app
  case app of
    VK -> application VK.Types.Pointer 
    Telegram -> application Telegram.Types.Pointer

application :: (App.Main pointer init _update) => pointer -> T () 
application pointer = do 
  setLogSettings (Blue, True, "application") 
  updateIdFromFile <- gets $ updateIdFromFile . configApp
  init <- App.getInit pointer
  if updateIdFromFile
      then do
          updateId <- toT getUpdateId
          logFuncT Info $ template "Получили updateId из файла: {0}" [show updateId] 
          longPolling pointer (App.setUpdateId init updateId)  --updateId из запроса перезаписываем тем, что из файла
      else do
          longPolling pointer init

longPolling :: (App.Main pointer init update) => pointer -> init -> T () 
longPolling pointer init = do 
  setLogSettings (Cyan, True, "longPolling")
  (updates, newInit) <-  App.getUpdates init
  updateIdFromFile <- gets $ updateIdFromFile . configApp
  when updateIdFromFile do 
      uid <- gets $ updateId . configApp
      let newuid = App.getUpdateId newInit
      toT $ setUpdateId newuid
      logFuncT Info $ template "Обновляем updateId в файле с {0} на {1}" [show uid, show newuid]
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






  


