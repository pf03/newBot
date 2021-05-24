module App.Main where

import qualified Logic.Bot as Bot (application)
import qualified Interface.MCache.Exports as Cache
import Transformer.Exports ( Transformer, run )
import qualified Telegram.Bot.Types as Telegram
import qualified VK.Bot.Types as VK

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