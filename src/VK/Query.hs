module VK.Query
  ( getLongPollServer,
    longPoll,
    sendMessage,
  )
where

import Common.Types( TimeOut, UserId, Label, Message) 
import Common.Functions(template, safeTail) 
import Common.Convert((<:>), jc) 
import Data.Either (rights)
import Interface.Class (MError)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MError.Exports as Error
import Network.HTTP.Simple (Query, QueryItem)
import qualified VK.Encode as Encode (contentUrl, keyboard)
import qualified VK.Update as Update
import qualified VK.API as API

-----------------------------External------------------------------------------
getLongPollServer :: Cache.Token -> Update.GroupId -> API.Version -> Query
getLongPollServer token groupId version =
  "group_id" <:> groupId
    ++ "access_token" <:> token
    ++ "v" <:> version

longPoll :: Update.Init -> TimeOut -> Query
longPoll ini timeout =
  "act" <:> ("a_check" :: String)
    ++ "key" <:> Update.key ini
    ++ "ts" <:> Update.ts ini
    ++ "wait" <:> timeout

sendMessage :: MError m => Cache.Token -> API.Version -> Update.Update -> [Label] -> m Query
sendMessage token version (cid, Update.Entity emc as) btns = do
  case emc of
    Right _ -> Error.throw $ Error.QueryError "Unable to send command to user"
    Left message -> do
      let qDefault = _queryDefault token cid version
      let qMessage = "message" <:> message
      let qButtons = if null btns then [] else "keyboard" <:> Encode.keyboard btns
      let qAttachments = _queryAttachments as
      return $ qDefault ++ qMessage ++ qButtons ++ qAttachments

-----------------------------Internal------------------------------------------
_queryDefault :: Cache.Token -> UserId -> API.Version -> Query
_queryDefault token userId version = "peer_id" <:> userId ++ "access_token" <:> token ++ "v" <:> version

_queryMessage :: Message -> Query
_queryMessage message = "message" <:> message

_queryAttachments :: [Update.Attachment] -> Query
_queryAttachments as = rights esqi ++ qStr
  where
    qStr = if null str then [] else "attachment" <:> str
    esqi = map _queryAttachment as
    str = safeTail $ foldl helper "" esqi
    helper :: String -> Either String QueryItem -> String
    helper value1 (Left value2) = template "{0},{1}" [value1, value2]
    helper value _ = value

_queryAttachment :: Update.Attachment -> Either String QueryItem
_queryAttachment attachment =
  case attachment of
    Update.Sticker stikerId -> Right ("sticker_id", jc stikerId)
    Update.Link url -> Right ("content_source", jc $ Encode.contentUrl url)
    Update.Audio ownerId objectId -> Left $ template "audio{0}_{1}" [show ownerId, show objectId]
    Update.Wall ownerId objectId -> Left $ template "wall{0}_{1}" [show ownerId, show objectId]
    Update.Item itemName ownerId objectId accessKey ->
      Left $
        template "{3}{0}_{1}_{2}" [show ownerId, show objectId, accessKey, itemName]