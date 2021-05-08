{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Logic.Bot where



-- Our modules
import qualified Logic.Logic as Logic
import Interface.MLog as Log
import Interface.MCache as Cache
import Interface.Messenger.IBot as Bot
import Common.Misc
import Interface.MT
import qualified Common.Color

-- Other modules
import qualified System.Console.ANSI as Color (Color(..)) 
import Control.Monad.State.Lazy
import Data.Maybe
import Control.Concurrent
--import GHC.Generics  --зачем это?

-- это уровень App
-- main:: IO()
-- main = do
--   exVar <- newMVar False
--   forkIO $ runT switchApplication
--   exit exVar

-- switchApplication :: T () 
-- switchApplication = do
--   app <- Cache.getApp
--   case app of
--     VK -> application VK.Pointer 
--     Telegram -> application Telegram.Pointer

application :: (MT m, IBot pointer init _update) => pointer -> m () 
application pointer = do 
  Log.setSettings (Color.Blue, True, "application") 
  updateIdFromFile <- Cache.getUpdateIdFromFile
  init <- Bot.getInit pointer
  if updateIdFromFile
      then do
          updateId <- Cache.getUpdateId
          Log.funcT Info $ template "Получили updateId из файла: {0}" [show updateId] 
          longPolling pointer (Bot.setUpdateId init updateId)  --updateId из запроса перезаписываем тем, что из файла
      else do
          longPolling pointer init

longPolling :: (MT m, IBot pointer init update) => pointer -> init -> m () 
longPolling pointer init = do 
  Log.setSettings (Color.Cyan, True, "longPolling")
  (updates, newInit) <-  Bot.getUpdates init
  updateIdFromFile <- Cache.getUpdateIdFromFile
  when updateIdFromFile do 
      uid <- Cache.getUpdateId
      let newuid = Bot.getUpdateId newInit
      Cache.setUpdateId newuid
      Log.funcT Info $ template "Обновляем updateId в файле с {0} на {1}" [show uid, show newuid]
      --saveST --updateId из файла перезаписываем тем, что из запроса
      Cache.writeCache
  calcSendMesages updates
  longPolling pointer newInit

--отвечаем всем пользователям
calcSendMesages :: (MT m, IBot _pointer _init update) => [update] -> m ()
calcSendMesages = mapM_ $ \update -> do 
  (stateChanged, answer, btns) <- Logic.answer update
  --обновляем конфиг
  when stateChanged $ do
      putStrLnT "Обновляем конфиг..."
      Cache.writeCache
  Bot.sendMessage answer btns

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






  


