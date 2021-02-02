
--importPriority = 30
module Logic where 
--import Data.List 
import qualified Data.Map.Internal as M

import Config --40
import Types --hiding (Config(..)) --100
--import Data.List
import Control.Monad.Reader
import Prelude hiding (repeat)
import qualified App  --60
import Common
import State
import Control.Monad.State.Lazy

toMessageCommand :: String -> Either Message Command
toMessageCommand str = let 
    w = words str;
    l = length w in
    case  l of 
        0 -> Left str  
        1 -> case head w of 
            "/help" -> Right Help
            "/repeat" -> Right Repeat
            "/start" -> Right Start
            '/':n:[] | n `elem` ("12345"::String) -> Right $ Button $ read [n]
            '/':x:xs | x /=' ' -> Right . Unknown . unwords $ (x:xs):tail w
            x -> Left str
        _ -> case head w of 
            '/':x:xs | x /=' ' -> Right . Unknown . unwords $ (x:xs):tail w
            x -> Left str

answer :: (App.Main _pointer _init update) => update -> State S (StateChanged, update, [Label])  
answer update = do
    let mmessage = App.getMessage update
    let mcommand = App.getCommand update
    let memc = maybeToEither mmessage mcommand
    let cid = App.getChatId update
    case memc of
        Just emc -> do
            (changed, newMessage, btns) <- answerMessageCommand cid emc
            return (changed, App.setMessage update newMessage, btns)  --ответ на команду или сообщение
        Nothing -> do
            return (False, update , []) --ответ по умолчанию

maybeToEither:: Maybe a -> Maybe b -> Maybe (Either a b)
maybeToEither ma mb = 
    case ma of 
        Just a -> Just $ Left a
        Nothing -> case mb of
            Just b -> Just $ Right b
            Nothing -> Nothing

--flag StateChanged needed in order to update the config later in a dirty function
answerMessageCommand :: ChatId -> Either Message Command -> State S (StateChanged, Message, [Label])  
answerMessageCommand cid emc = do
    text <- textAnswer cid emc
    labels <- case emc of
        Right Repeat -> return $ map (('/':). show) [1..5]
        _ -> return []
    case emc of
        Right (Button n) -> do
            --setRepeatNumber cid n --remove to textAnswer for tests
            return (True, text, labels)
        _ -> return (False , text, labels)

textAnswer :: ChatId -> Either Message Command -> State S Message 
textAnswer cid emc = do 
    ConfigText helpText repeatText unknownText buttonText <- getConfigText
    mrn <- getmRepeatNumber cid
    case emc of
        Left message -> do
            case mrn of 
                Nothing -> return message
                Just n -> return  . safeInit . concat . replicate n $ (message ++ " ")
        Right Help -> return helpText
        Right Start -> return helpText 
        Right Repeat -> return $ template repeatText [show mrn]
        Right (Button n) -> do
            setRepeatNumber cid n
            return $ template buttonText [show mrn, show n]
        Right (Unknown com) -> return $ template unknownText [com]




