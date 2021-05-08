{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE BangPatterns #-}
--importPriority = 11
module VK.Query 
--(getLongPollServer, longPoll, sendMessage, test) 
where

-- Our modules
import Logic.Parse ( (<:>), (<:?>) )
import Interface.MError as Error
import Interface.MCache as Cache
import Common.Misc

-- Other modules
import  VK.Update as Update --60
-- import Types --100
-- import VK.Types --99
import VK.Parse
import qualified VK.Parse as Encode (keyboard, contentUrl, contentMessage) --49
import Data.Either
import Network.HTTP.Simple
import Control.Monad.Trans.Except
import GHC.Generics
import Data.Aeson


-----------------------------Types---------------------------------------------
type Version = String 
type TimeOut = Int   --таймаут для long polling
-- data Init = Init {server :: String, key :: String, ts :: Int } deriving (Show, Generic)  --могут быть orchan instances для IBot
-- instance FromJSON Init
-- instance ToJSON Init

--можно добавить сюда интерфейс MCache
--------------------------------------------External------------------------------------------------------------------
getLongPollServer :: Token  -> GroupId -> Version -> Query 
getLongPollServer token groupId version = "group_id" <:> groupId ++ "access_token" <:> token ++ "v" <:> version

longPoll :: Init -> TimeOut -> Query
longPoll init timeout = "act" <:> ("a_check" :: String) ++ "key" <:> key init ++ "ts" <:> ts init ++ "wait" <:> timeout

sendMessage :: MError m => Token -> Version -> Update -> [Label] -> m Query 
sendMessage token version (cid, Entity emc attachments) btns = do
    case emc of 
        Right command -> Error.throw $ QueryError "Невозможно послать команду пользователю"
        -- Message m -> do
        Left message -> do
            let qDefault = _queryDefault token cid version
            let qMessage =  "message" <:> message
            let qButtons = if null btns then [] else "keyboard" <:> Encode.keyboard btns 
            let qAttachments = _queryAttachments attachments
            return $ qDefault ++ qMessage ++ qButtons ++ qAttachments

--------------------------------------------Internal------------------------------------------------------------------
_queryDefault :: Token -> UserId -> Version -> Query 
_queryDefault token userId version = "peer_id" <:> userId ++ "access_token" <:> token ++ "v" <:> version 

_queryMessage :: Message -> Query
_queryMessage message = "message" <:> message

_queryAttachments :: [Attachment] -> Query
_queryAttachments attachments =  rights esqi ++ qStr where
    qStr = if null str then [] else "attachment" <:> str
    esqi = map _queryAttachment attachments
    str = safeTail $ foldl helper "" esqi
    helper :: String -> Either String QueryItem -> String
    helper value1 (Left value2) = template "{0},{1}" [value1, value2]  --можно использовать lefts
    helper value _ = value

_queryAttachment :: Attachment -> Either String QueryItem  --Either String Query
_queryAttachment attachment = 
    case attachment of
        Sticker stikerId -> Right ("sticker_id", jc stikerId)
        Link url -> Right ("content_source", jc $ Encode.contentUrl url)
        --Link url -> Right ("content_source", App.jc $ contentMessage mgroup mgroup 100 )
        Audio ownerId objectId  -> Left $ template "audio{0}_{1}" [show ownerId, show objectId]
        Wall ownerId objectId  -> Left $ template "wall{0}_{1}" [show ownerId, show objectId]
        Item itemName ownerId objectId accessKey -> Left $ template "{3}{0}_{1}_{2}" [show ownerId, show objectId, accessKey, itemName]
        --думаю, если не отправляет, то как то связано с правами доступа, возможно он пытается отправить файл не собственнику, и перепроверить все id, которые он отправляет


--------------------------------------------Test------------------------------------------------------------------------
roman = 37096463
kesha = 611508530
group = 201551107
mgroup = -group

test :: Token -> Version -> Query
test token version = q ++ qDefault ++ qMessage where 
    cid = roman
    qDefault = _queryDefault token cid version
    qMessage = _queryMessage "link"
    --q = _queryAttachments [testWall1]
    q=_queryAttachments [Link url3]

url = "https://m.vk.com/poll-201551107_494468876"

url1 = "https://vk.com/feed?w=poll-201551107_494468876"

url2="https://vk.com/poll-201551107_494468876"

url3= "https://vk.com/albums37096463?z=photo37096463_456239075%2Fphotos37096463"


-- freeQuery :: Query 
-- freeQuery = undefined


--roman-group-bot
testWall1  = Wall mgroup 11