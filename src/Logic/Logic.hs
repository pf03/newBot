module Logic.Logic where

import Common.Functions (template)
import Common.Types ( ChatId, Command(..), Label, Message )
import Interface.Class (IUpdate, MCache)
import qualified Interface.MCache.Exports as Cache
import qualified Messenger.Update.Class as Update

toMessageCommand :: String -> Either Message Command
toMessageCommand str =
  let words0 = words str
      length0 = length words0
   in case length0 of
        0 -> Left str
        1 -> case head words0 of
          "/help" -> Right Help
          "/repeat" -> Right Repeat
          "/start" -> Right Start
          ['/', n] | n `elem` ("12345" :: String) -> Right $ Button $ read [n]
          '/' : x : xs | x /= ' ' -> Right . Unknown . unwords $ (x : xs) : tail words0
          _ -> Left str
        _ -> case head words0 of
          '/' : x : xs | x /= ' ' -> Right . Unknown . unwords $ (x : xs) : tail words0
          _ -> Left str

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
  Cache.ConfigText helpText repeatText unknownText buttonText <- Cache.getConfigText
  repeatNumber <- Cache.getRepeatNumber chatId
  case command of
    Help -> return (helpText, [])
    Start -> return (helpText, [])
    Repeat -> return (template repeatText [show repeatNumber], map (('/' :) . show) [1 :: Int .. 5])
    Button n -> do
      Cache.setRepeatNumber chatId n
      return (template buttonText [show repeatNumber, show n],[])
    Unknown com -> return (template unknownText [com],[])