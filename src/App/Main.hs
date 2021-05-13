module App.Main where

-- Our modules
import Interface.MError as Error
import           Logic.Bot          as Bot
import           Logic.Config       as Config
import           T.State            as S hiding (app)
import           T.Transformer
import           Telegram.Bot       as Telegram
import           VK.Bot             as VK

-- Other modules
import           Control.Concurrent
import Control.Exception.Base

main_:: IO()
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
        VK       -> Bot.application VK.Pointer
        Telegram -> Bot.application Telegram.Pointer 

exit :: MVar ThreadId -> IO ()
exit appIdMVar = do
    putStrLn "Press q to exit..."
    line <- getLine
    if line == "q" then do
        putStrLn "Exit from application when long polling request ends ..."
        appId <- takeMVar appIdMVar
        throwTo appId UserInterrupt
    else do
        putStrLn "Wrong command"
        exit appIdMVar