module App.Main where

import qualified Logic.Bot as Bot (application)
import Logic.Config.Types (App (Telegram, VK))
import Transformer.State (Transformer)
import qualified Transformer.State as S (getApp)
import Transformer.Run (runT)
import qualified Telegram.Bot.Types as Telegram
import qualified VK.Bot.Types as VK

main_ :: IO ()
main_ = do
  runT switchApplication
  putStrLn "Press enter for exit..."
  _ <- getLine
  return ()

switchApplication :: Transformer ()
switchApplication = do
  app <- S.getApp
  case app of
    VK -> Bot.application VK.Pointer
    Telegram -> Bot.application Telegram.Pointer