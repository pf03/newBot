module Main where

import Class (MTrans)
import qualified Interface.Cache.Config.Exports as Config
import qualified Interface.Cache.Exports as Cache
import qualified Logic.App as App
import qualified Messenger.Bot.Telegram.Types as Telegram
import qualified Messenger.Bot.VK.Types as VK
import qualified Transformer.Exports as Transformer
import qualified Interface.Error.Exports as Error
import qualified Interface.Log.Exports as Log
import Control.Exception --( catch )
import Control.Monad.IO.Class

main :: IO ()
main = do
  Transformer.run $ errorHandle switchApplication
  -- Transformer.run switchApplication
  putStrLn "Press enter for exit..."
  _ <- getLine
  return ()

-- errorHandler :: Error.Error -> IO()
-- errorHandler err  = do
--   putStrLn "Application error1:"
--   putStrLn $ show err
--   return() 

switchApplication :: MTrans m => m ()
switchApplication = do
  app <- Cache.getApp
  case app of
    Config.VK -> App.runApplication VK.Pointer
    Config.Telegram -> App.runApplication Telegram.Pointer

errorHandle :: MTrans m => m () -> m ()
errorHandle m = do
  Log.writeErrorM "error handle1: "
  x <- m
  Log.writeErrorM "error handle2: "
  ex <- liftIO $ try (evaluate x) :: MTrans m => m (Either Error.Error ())
  case ex of 
    Right () -> do
      Log.writeErrorM "No errors: "
      return()
    Left err -> do
      Log.writeErrorM "Application error2: "
      Log.writeErrorM $ show err

-- errorHandle2 :: IO () -> IO ()
-- errorHandle2 m = do
--   Log.writeErrorM "error handle1: "
--   x <- m
--   Log.writeErrorM "error handle2: "
--   ex <- liftIO $ try (evaluate x) :: MTrans m => m (Either Error.Error ())
--   case ex of 
--     Right () -> do
--       Log.writeErrorM "No errors: "
--       return()
--     Left err -> do
--       Log.writeErrorM "Application error2: "
--       Log.writeErrorM $ show err


  