module App.Main where

import Control.Concurrent (MVar, ThreadId, forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay, throwTo)
import Control.Exception.Base (AsyncException (ThreadKilled, UserInterrupt), finally)
import qualified Logic.Bot as Bot (application)
import Logic.Config (App (Telegram, VK))
import T.State (T)
import qualified T.State as S (getApp)
import T.Transformer (runT)
import qualified Telegram.Bot as Telegram
import qualified VK.Bot as VK

main_ :: IO ()
main_ = do
  appExitMVar <- newEmptyMVar
  appIdMVar <- newEmptyMVar
  exitId <- forkIO $ exit appIdMVar

  threadDelay 2000000
  appId <- forkIO $ do
    finally (runT switchApplication) $ do
      putStrLn "Application closed"
      throwTo exitId ThreadKilled
      putMVar appExitMVar ()
  putMVar appIdMVar appId
  takeMVar appExitMVar
  putStrLn "Press enter for exit..."
  _ <- getLine
  return ()

switchApplication :: T ()
switchApplication = do
  app <- S.getApp
  case app of
    VK -> Bot.application VK.Pointer
    Telegram -> Bot.application Telegram.Pointer

exit :: MVar ThreadId -> IO ()
exit appIdMVar = do
  putStrLn "Press q to exit..."
  line <- getLine
  if line == "q"
    then do
      putStrLn "Exit from application when long polling request ends ..."
      appId <- takeMVar appIdMVar
      throwTo appId UserInterrupt
    else do
      putStrLn "Wrong command"
      exit appIdMVar