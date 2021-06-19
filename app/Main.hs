module Main where

import Class (MTrans)
import qualified Interface.Cache.Config.Exports as Config
import qualified Interface.Cache.Exports as Cache
import qualified Logic.App as App (application)
import qualified Messenger.Bot.Telegram.Types as Telegram
import qualified Messenger.Bot.VK.Types as VK
import qualified Transformer.Exports as Transformer

main :: IO ()
main = do
  Transformer.run switchApplication
  putStrLn "Press enter for exit..."
  _ <- getLine
  return ()

switchApplication :: MTrans m => m ()
switchApplication = do
  app <- Cache.getApp
  case app of
    Config.VK -> App.application VK.Pointer
    Config.Telegram -> App.application Telegram.Pointer