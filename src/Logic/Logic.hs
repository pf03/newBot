module Logic.Logic where

import Common.Types (ChatId, Command (..), Label, Message)
import Common.Functions (safeInit, template)
import Interface.Class (IUpdate, MCache)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.Messenger.IUpdate as Update

toMessageCommand :: String -> Either Message Command
toMessageCommand str =
  let w = words str
      l = length w
   in case l of
        0 -> Left str
        1 -> case head w of
          "/help" -> Right Help
          "/repeat" -> Right Repeat
          "/start" -> Right Start
          ['/', n] | n `elem` ("12345" :: String) -> Right $ Button $ read [n]
          '/' : x : xs | x /= ' ' -> Right . Unknown . unwords $ (x : xs) : tail w
          _ -> Left str
        _ -> case head w of
          '/' : x : xs | x /= ' ' -> Right . Unknown . unwords $ (x : xs) : tail w
          _ -> Left str

answer :: (MCache m, IUpdate update) => update -> m (update, [Label])
answer update = do
  let mmessage = Update.getMessage update
  let mcommand = Update.getCommand update
  let memc = maybeToEither mmessage mcommand
  let cid = Update.getChatId update
  case memc of
    Just emc -> do
      (newMessage, btns) <- answerMessageCommand cid emc
      return (Update.setMessage update newMessage, btns) -- answer to message or command
    Nothing -> do
      return (update, []) -- default answer

maybeToEither :: Maybe a -> Maybe b -> Maybe (Either a b)
maybeToEither ma mb =
  case ma of
    Just a -> Just $ Left a
    Nothing -> case mb of
      Just b -> Just $ Right b
      Nothing -> Nothing

answerMessageCommand :: MCache m => ChatId -> Either Message Command -> m (Message, [Label])
answerMessageCommand cid emc = do
  text <- textAnswer cid emc
  labels <- case emc of
    Right Repeat -> return $ map (('/' :) . show) [1 :: Int .. 5]
    _ -> return []
  return (text, labels)

textAnswer :: MCache m => ChatId -> Either Message Command -> m Message
textAnswer cid emc = do
  Cache.ConfigText helpText repeatText unknownText buttonText <- Cache.getConfigText
  rn <- Cache.getRepeatNumber cid
  case emc of
    Left message -> return . safeInit . concat . replicate rn $ (message ++ " ")
    Right Help -> return helpText
    Right Start -> return helpText
    Right Repeat -> return $ template repeatText [show rn]
    Right (Button n) -> do
      Cache.setRepeatNumber cid n
      return $ template buttonText [show rn, show n]
    Right (Unknown com) -> return $ template unknownText [com]
