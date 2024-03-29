module Main where

import qualified Interface.Cache.Config.Exports as Config
import qualified Interface.Cache.Exports as Cache
import qualified Logic.App as App
import qualified Messenger.Bot.Telegram.Instances as Telegram
import qualified Messenger.Bot.VK.Instances as VK
import qualified Transformer.Exports as Transformer
import Transformer.Types (BotStateIO)

main :: IO ()
main = do
  Transformer.run switchApplication
  putStrLn "Press enter for exit..."
  _ <- getLine
  return ()

switchApplication :: BotStateIO ()
switchApplication = do
  app <- Cache.getApp
  case app of
    Config.VK -> App.runApplication VK.Pointer
    Config.Telegram -> App.runApplication Telegram.Pointer