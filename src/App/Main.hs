module App.Main where

import qualified Logic.Bot as Bot (application)
import Logic.Config (App (Telegram, VK))
import T.State (T)
import qualified T.State as S (getApp)
import T.Transformer (runT)
import qualified Telegram.Bot as Telegram
import qualified VK.Bot as VK

main_ :: IO ()
main_ = do
  runT switchApplication
  putStrLn "Press enter for exit..."
  _ <- getLine
  return ()

switchApplication :: T ()
switchApplication = do
  app <- S.getApp
  case app of
    VK -> Bot.application VK.Pointer
    Telegram -> Bot.application Telegram.Pointer