
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
import Control.Monad.State.Lazy

--используется в Parse
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
            '/':x:xs | x /=' ' -> Right . Unknown $ (x:xs)
            x -> Left str
        _ -> case head w of 
            '/':x:xs | x /=' ' -> Right . Unknown $ (x:xs)
            x -> Left str


--этот тип мы поместили здесь во избежание конфликтов с типом Update, у которого такие же конструкторы
--data Env update = Env {configApp :: ConfigApp, configText:: ConfigText, update :: update}

--обертка для удобства внешнего вызова это будет делать функция toT
-- runAnswer :: (App.Main _pointer _init update) =>  Config -> update -> Action
-- runAnswer configApp configText u = runReader answer $ Env configApp configText u

--глобальная функция для передачи результата, возможно здесь будет другой тип вместо update, типа answer
-- answer :: (App.Main _pointer _init update) => update -> Reader S (Maybe ConfigApp, update, [Label])  
-- answer update = do
--     let mmessage = App.getMessage update
--     let mcommand = App.getCommand update
--     let memc = maybeToEither mmessage mcommand
--     let cid = App.getChatId update
--     case memc of
--         Just emc -> do
--             (mcapp, newMessage, btns) <- answerMessageCommand cid emc
--             return (mcapp, App.setMessage update newMessage, btns)  --ответ на команду или сообщение
--         Nothing -> do
--             return (Nothing, update , []) --ответ по умолчанию

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



--сюда нужно передавать в идеале всю историю переписки и id чата для работы с конфигом
--основная функция, решает 1- перезаписать конфиг (если Nothing, то оставляем как есть),  2 - что ответить, 3 - какие посылать кнопки
-- Either Message Command !!
--тут Reader слегка кривоват, при рефакторинге либо убрать его, либо заменить кривой тип Env на Config или (Config, Update)
--эта функция может обработать только сообщение или команду. Остальные сущности будут обрабатываться в VK или Telegram соответственно
-- answerMessageCommand :: ChatId -> Either Message Command -> Reader S Action  
-- answerMessageCommand cid emc = do
--     case emc of
--         Left message -> repeatMessage cid message
--         Right Help -> helpAnswer
--         Right Start -> helpAnswer--это начальное сообщение боту
--         Right Repeat -> repeatAnswer cid
--         Right (Button n) -> buttonAnswer cid n
--         Right (Unknown com) -> unknownAnswer (Unknown com)

-- --ответ на /help, не зависит от нашего сообщения
-- helpAnswer :: Reader S Action
-- helpAnswer = do
--     ht <- asks $ help . configText
--     return (Nothing, ht, [])

-- --ответ на /repeat, не зависит от нашего сообщения
-- repeatAnswer :: ChatId -> Reader S Action 
-- repeatAnswer cid = do 
--     n <- asks $ show . M.lookup cid . repeatNumber . configApp
--     rt <- asks $ repeat . configText
--     return (Nothing, template rt [n], map (('/':). show) [1..5])

-- --ответ на нажатие кнопки
-- buttonAnswer :: ChatId -> Int -> Reader S Action
-- buttonAnswer cid n = do
--     rn <- asks $ repeatNumber . configApp;
--     bt <- asks $ button . configText;
--     ca <- asks configApp;
--     let newca = ca {repeatNumber = M.insert cid n rn};
--     let oldn = M.lookup cid rn
--     return (Just newca, template bt [show oldn, show n], [])

-- --ответ на /???
-- unknownAnswer :: Command -> Reader S Action
-- unknownAnswer (Unknown com) = do
--     ut <- asks $ unknown . configText;
--     return (Nothing, template ut [com], [])

-- --ответ на произвольное сообщение
-- repeatMessage ::  ChatId -> Message -> Reader S Action
-- repeatMessage cid text = do
--     mn <- asks $ M.lookup cid . repeatNumber . configApp 
--     case mn of 
--         Nothing -> return (Nothing, text, [])
--         Just n -> return (Nothing, concat $ replicate n (text ++ " "), [])

--возможно такое распределение будет более читаемо и тестируемо, но не факт
--флаг StateChanged нужен для того, чтобы потом в грязной функции обновить конфиг
answerMessageCommand :: ChatId -> Either Message Command -> State S (StateChanged, Message, [Label])  
answerMessageCommand cid emc = do
    text <- textAnswer cid emc
    labels <- case emc of
        Right Repeat -> return $ map (('/':). show) [1..5]
        _ -> return []
    case emc of
        Right (Button n) -> do
            setRepeatNumber cid n
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
                Just n -> return  . concat . replicate n $ (message ++ " ")
        Right Help -> return helpText
        Right Start -> return helpText --это начальное сообщение боту
        Right Repeat -> return $ template repeatText [show mrn]
        Right (Button n) -> return $ template buttonText [show mrn, show n]
        Right (Unknown com) -> return $ template unknownText [com]

testingFunction = head



