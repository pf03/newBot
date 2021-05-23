module App.Main where

import qualified Logic.Bot as Bot (application)
import qualified Interface.MCache.Exports as Cache
import qualified Logic.Config.Exports as Config
import Transformer.Exports
import qualified Telegram.Bot.Types as Telegram
import qualified VK.Bot.Types as VK
-- import Control.Concurrent
-- import Control.Monad (forM_)

-- сделать что-то типа runFork, runFirst, чтобы абстрагироваться от типа State
-- main_ :: IO ()
-- main_ = do
--   mc <- runConfig Config.readConfig
--   case mc of 
--     Nothing -> return ()
--     Just c -> do
--       let ss = configToStates c
--       forM_ ss $ \s -> forkIO (run switchApplication s)
--       -- putStrLn "Press enter for exit..."
--       -- _ <- getLine
--       -- return ()

main_ :: IO ()
main_ = do
  run switchApplication
  putStrLn "Press enter for exit..."
  _ <- getLine
  return ()

switchApplication :: Transformer ()
switchApplication = do
  app <- Cache.getApp
  case app of
    Cache.VK -> Bot.application VK.Pointer
    Cache.Telegram -> Bot.application Telegram.Pointer