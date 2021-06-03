module App.Main where

import qualified Logic.Bot as Bot (application)
import qualified Interface.MCache.Exports as Cache
import Interface.Class ( MTrans)
import qualified Transformer.Exports as Transformer
import qualified Messenger.Bot.Telegram.Types as Telegram
import qualified Messenger.Bot.VK.Types as VK

main_ :: IO ()
main_ = do
  Transformer.run switchApplication
  putStrLn "Press enter for exit..."
  _ <- getLine
  return ()

switchApplication :: MTrans m => m ()
switchApplication = do
  app <- Cache.getApp
  case app of
    Cache.VK -> Bot.application VK.Pointer
    Cache.Telegram -> Bot.application Telegram.Pointer