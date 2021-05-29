module Logic.Logic where

import Common.Functions (template)
import Common.Types (ChatId, Command (..), Label, Message)
import qualified Data.Bifunctor as Bifunctor
import Interface.Class (IUpdate, MCache)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.Messenger.IUpdate as Update

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

answer :: (MCache m, IUpdate update) => update -> m [(update, [Label])]
answer update = do
  let mmessage = Update.getMessage update
  let mcommand = Update.getCommand update
  let meMessageCommand = maybeToEither mmessage mcommand
  let chatId = Update.getChatId update
  case meMessageCommand of
    Just eMessageCommand -> do
      list <- answerMessageCommand chatId eMessageCommand
      return $ map (Bifunctor.first (Update.setMessage update)) list -- answer to message or command
    Nothing -> do
      repeatNumber <- Cache.getRepeatNumber chatId
      return $ replicate repeatNumber (update, []) -- default answer
  where
    maybeToEither :: Maybe a -> Maybe b -> Maybe (Either a b)
    maybeToEither ma mb =
      case ma of
        Just a -> Just $ Left a
        Nothing -> case mb of
          Just b -> Just $ Right b
          Nothing -> Nothing

answerMessageCommand :: MCache m => ChatId -> Either Message Command -> m [(Message, [Label])]
answerMessageCommand chatId eMessageCommand = do
  texts <- textAnswer chatId eMessageCommand
  labels <- case eMessageCommand of
    Right Repeat -> return $ map (('/' :) . show) [1 :: Int .. 5]
    _ -> return []
  return $ zip texts $ repeat labels

textAnswer :: MCache m => ChatId -> Either Message Command -> m [Message]
textAnswer chatId eMessageCommand = do
  Cache.ConfigText helpText repeatText unknownText buttonText <- Cache.getConfigText
  repeatNumber <- Cache.getRepeatNumber chatId
  case eMessageCommand of
    Left message -> return $ replicate repeatNumber message
    Right Help -> return [helpText]
    Right Start -> return [helpText]
    Right Repeat -> return [template repeatText [show repeatNumber]]
    Right (Button n) -> do
      Cache.setRepeatNumber chatId n
      return [template buttonText [show repeatNumber, show n]]
    Right (Unknown com) -> return [template unknownText [com]]