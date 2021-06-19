module Logic.Logic where

import Class (IUpdate, MCache)
import Common.Functions (template)
import Common.Types (ChatId, Command (..), Label (..), Message (..))
import qualified Interface.Cache.Config.Exports as Config
import qualified Interface.Cache.Exports as Cache
import qualified Messenger.Update.Class as Update

toMessageCommand :: String -> Either Message Command
toMessageCommand str =
  let words0 = words str
      length0 = length words0
   in case length0 of
        0 -> Left $ Message str
        1 -> case head words0 of
          "/help" -> Right Help
          "/repeat" -> Right Repeat
          "/start" -> Right Start
          ['/', n] | n `elem` ("12345" :: String) -> Right $ Button $ read [n]
          '/' : x : xs | x /= ' ' -> Right . Unknown . unwords $ (x : xs) : tail words0
          _ -> Left $ Message str
        _ -> case head words0 of
          '/' : x : xs | x /= ' ' -> Right . Unknown . unwords $ (x : xs) : tail words0
          _ -> Left $ Message str

evalAnswer :: (MCache m, IUpdate update) => update -> m (update, [Label], Int)
evalAnswer update = do
  let chatId = Update.getChatId update
  repeatNumber <- Cache.getRepeatNumber chatId
  case Update.getCommand update of
    Just command -> do
      (message, btns) <- evalCommandAnswer chatId command
      return (Update.setMessage update message, btns, 1)
    Nothing -> return (update, [], repeatNumber)

evalCommandAnswer :: (MCache m) => ChatId -> Command -> m (Message, [Label])
evalCommandAnswer chatId command = do
  Config.ConfigText helpText repeatText unknownText buttonText <- Cache.getConfigText
  repeatNumber <- Cache.getRepeatNumber chatId
  case command of
    Help -> return (Message helpText, [])
    Start -> return (Message helpText, [])
    Repeat -> return (Message $ template repeatText [show repeatNumber], map (Label . ('/' :) . show) [1 :: Int .. 5])
    Button n -> do
      Cache.setRepeatNumber chatId n
      return (Message $ template buttonText [show repeatNumber, show n], [])
    Unknown com -> return (Message $ template unknownText [com], [])