

module App.Main where

import           Control.Concurrent
import           Logic.Bot          as Bot
import           Logic.Config       as Config
import           T.State            as S
import           T.Transformer
import           Telegram.Bot       as Telegram
import           VK.Bot             as VK

main:: IO()
main = do
  exVar <- newMVar False  --это не работает, не дожидается конца запроса
  forkIO $ runT switchApplication
  exit exVar

switchApplication :: T ()
switchApplication = do
  app <- S.getApp
  case app of
    VK       -> Bot.application VK.Pointer
    Telegram -> Bot.application Telegram.Pointer

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
