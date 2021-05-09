module App.Main where

-- Our modules
import           Logic.Bot          as Bot
import           Logic.Config       as Config
import           T.State            as S hiding (app)
import           T.Transformer
import           Telegram.Bot       as Telegram
import           VK.Bot             as VK

-- Other modules
import           Control.Concurrent

main:: IO()
main = do
  exVar <- newMVar False  --это не работает, не дожидается конца запроса
  _ <- forkIO $ runT switchApplication
  exit exVar

switchApplication :: T ()
switchApplication = do
  app <- S.getApp
  case app of
    VK       -> Bot.application VK.Pointer
    Telegram -> Bot.application Telegram.Pointer

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
