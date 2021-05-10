module VK.Query
(getLongPollServer, longPoll, sendMessage)
where

-- Our modules
import           Common.Misc
import           Interface.MCache           as Cache hiding (token)
import           Interface.MError           as Error

-- Other modules
import           VK.Update                  as Update
import           Data.Either
import           Network.HTTP.Simple
import           VK.Parse
import qualified VK.Parse                   as Encode (contentUrl, keyboard)


-----------------------------Types---------------------------------------------
type Version = String

-----------------------------External------------------------------------------
getLongPollServer :: Token  -> GroupId -> Version -> Query
getLongPollServer token groupId version = "group_id" <:> groupId ++ 
    "access_token" <:> token ++ 
    "v" <:> version

longPoll :: Init -> TimeOut -> Query
longPoll init timeout = "act" <:> ("a_check" :: String) ++ 
    "key" <:> key init ++ 
    "ts" <:> ts init ++ 
    "wait" <:> timeout

sendMessage :: MError m => Token -> Version -> Update -> [Label] -> m Query
sendMessage token version (cid, Entity emc attachments) btns = do
    case emc of
        Right command -> Error.throw $ QueryError "Unable to send command to user"
        Left message -> do
            let qDefault = _queryDefault token cid version
            let qMessage =  "message" <:> message
            let qButtons = if null btns then [] else "keyboard" <:> Encode.keyboard btns
            let qAttachments = _queryAttachments attachments
            return $ qDefault ++ qMessage ++ qButtons ++ qAttachments

-----------------------------Internal------------------------------------------
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
    helper value1 (Left value2) = template "{0},{1}" [value1, value2]
    helper value _              = value

_queryAttachment :: Attachment -> Either String QueryItem 
_queryAttachment attachment =
    case attachment of
        Sticker stikerId -> Right ("sticker_id", jc stikerId)
        Link url -> Right ("content_source", jc $ Encode.contentUrl url)
        Audio ownerId objectId  -> Left $ template "audio{0}_{1}" [show ownerId, show objectId]
        Wall ownerId objectId  -> Left $ template "wall{0}_{1}" [show ownerId, show objectId]
        Item itemName ownerId objectId accessKey -> Left $ 
            template "{3}{0}_{1}_{2}" [show ownerId, show objectId, accessKey, itemName]