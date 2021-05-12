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

main_:: IO()
main_ = do
    userExitMVar <- newMVar False
    appExitMVar <- newEmptyMVar
    _ <- forkIO $ exit userExitMVar
    threadDelay 2000000
    _ <- forkIO $ do
        runT (switchApplication userExitMVar)
        putStrLn "Application closed"
        putMVar appExitMVar ()
    takeMVar appExitMVar
    putStrLn "Press enter for exit..."
    _ <- getLine
    return ()

switchApplication :: MVar Bool -> T ()
switchApplication userExitMVar = do
    app <- S.getApp
    case app of
        VK       -> Bot.application VK.Pointer userExitMVar
        Telegram -> Bot.application Telegram.Pointer userExitMVar 

exit :: MVar Bool -> IO ()
exit userExitMVar = do
    putStrLn "Press q to exit..."
    line <- getLine
    if line == "q" then do
        _ <- takeMVar userExitMVar
        putMVar userExitMVar True
        putStrLn "Exit from application when long polling request ends..."
    else do
        putStrLn "Wrong command"
        exit userExitMVar