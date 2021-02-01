--{-# LANGUAGE BangPatterns #-}
--importPriority = 11
module VK.API where
import qualified App --60
import Types --100
import VK.Types --99
import VK.Parse --49
import Parse
import Error
import Logic
import Common
import Class
import Data.Either


import Network.HTTP.Simple
import Control.Monad.Trans.Except

queryGetLongPollServer :: Token  -> GroupId -> Version -> Query 
queryGetLongPollServer token groupId version = "group_id" <:> groupId ++ "access_token" <:> token ++ "v" <:> version

queryLongPoll :: Init -> TimeOut -> Query
queryLongPoll init timeout = "act" <:> ("a_check" :: String) ++ "key" <:> key init ++ "ts" <:> ts init ++ "wait" <:> timeout

querySendMessage1 :: Token -> Version -> Update -> [Label] -> Except E Query 
querySendMessage1 token version (cid, Entity emc attachments) btns = do
    case emc of 
        Right command -> throwE $ QueryError "Невозможно послать команду пользователю"
        -- Message m -> do
        Left message -> do
            let qDefault = _queryDefault token cid version
            let qMessage =  "message" <:> message
            let qButtons = if null btns then [] else "keyboard" <:> keyboard btns 
            let qAttachments = _queryAttachments attachments
            return $ qDefault ++ qMessage ++ qButtons ++ qAttachments

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
        Link url -> Right ("content_source", jc $ contentUrl url)
        --Link url -> Right ("content_source", App.jc $ contentMessage mgroup mgroup 100 )
        Audio ownerId objectId  -> Left $ template "audio{0}_{1}" [show ownerId, show objectId]
        Wall ownerId objectId  -> Left $ template "wall{0}_{1}" [show ownerId, show objectId]
        Item itemName ownerId objectId accessKey -> Left $ template "{3}{0}_{1}_{2}" [show ownerId, show objectId, accessKey, itemName]
        --думаю, если не отправляет, то как то связано с правами доступа, возможно он пытается отправить файл не собственнику, и перепроверить все id, которые он отправляет



roman = 37096463
kesha = 611508530
group = 201551107
mgroup = -group

testQuery :: Token -> Version -> Query
testQuery token version = q ++ qDefault ++ qMessage where 
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


