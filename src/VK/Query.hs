module VK.Query
  ( getLongPollServer,
    longPoll,
    sendMessage,
  )
where

import Common.Misc (Label, Message, TimeOut, UserId, jc, safeTail, template)
import Data.Either (rights)
import Interface.MCache (Token)
import Interface.MError (E (QueryError), MError)
import qualified Interface.MError as Error (throw)
import Network.HTTP.Simple (Query, QueryItem)
import VK.Parse (Init (key, ts), (<:>))
import qualified VK.Parse as Encode (contentUrl, keyboard)
import VK.Update (Attachment (..), Entity (Entity), GroupId, Update)

-----------------------------Types---------------------------------------------
type Version = String

-----------------------------External------------------------------------------
getLongPollServer :: Token -> GroupId -> Version -> Query
getLongPollServer token groupId version =
  "group_id" <:> groupId
    ++ "access_token" <:> token
    ++ "v" <:> version

longPoll :: Init -> TimeOut -> Query
longPoll ini timeout =
  "act" <:> ("a_check" :: String)
    ++ "key" <:> key ini
    ++ "ts" <:> ts ini
    ++ "wait" <:> timeout

sendMessage :: MError m => Token -> Version -> Update -> [Label] -> m Query
sendMessage token version (cid, Entity emc as) btns = do
  case emc of
    Right _ -> Error.throw $ QueryError "Unable to send command to user"
    Left message -> do
      let qDefault = _queryDefault token cid version
      let qMessage = "message" <:> message
      let qButtons = if null btns then [] else "keyboard" <:> Encode.keyboard btns
      let qAttachments = _queryAttachments as
      return $ qDefault ++ qMessage ++ qButtons ++ qAttachments

-----------------------------Internal------------------------------------------
_queryDefault :: Token -> UserId -> Version -> Query
_queryDefault token userId version = "peer_id" <:> userId ++ "access_token" <:> token ++ "v" <:> version

_queryMessage :: Message -> Query
_queryMessage message = "message" <:> message

_queryAttachments :: [Attachment] -> Query
_queryAttachments as = rights esqi ++ qStr
  where
    qStr = if null str then [] else "attachment" <:> str
    esqi = map _queryAttachment as
    str = safeTail $ foldl helper "" esqi
    helper :: String -> Either String QueryItem -> String
    helper value1 (Left value2) = template "{0},{1}" [value1, value2]
    helper value _ = value

_queryAttachment :: Attachment -> Either String QueryItem
_queryAttachment attachment =
  case attachment of
    Sticker stikerId -> Right ("sticker_id", jc stikerId)
    Link url -> Right ("content_source", jc $ Encode.contentUrl url)
    Audio ownerId objectId -> Left $ template "audio{0}_{1}" [show ownerId, show objectId]
    Wall ownerId objectId -> Left $ template "wall{0}_{1}" [show ownerId, show objectId]
    Item itemName ownerId objectId accessKey ->
      Left $
        template "{3}{0}_{1}_{2}" [show ownerId, show objectId, accessKey, itemName]
