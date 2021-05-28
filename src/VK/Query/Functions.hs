module VK.Query.Functions where

import VK.Query.Internal ( queryDefault, queryAttachments ) 
import Common.Types ( TimeOut, Label )
import Common.Convert((<:>)) 
import Interface.Class (MError, MCache)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MError.Exports as Error
import Network.HTTP.Simple (Query)
import qualified VK.Encode as Encode (keyboard)
import qualified VK.Update as Update

getLongPollServer :: MCache m => m Query
getLongPollServer = do
  groupId <- Cache.getGroupId
  token <- Cache.getToken
  version <- Cache.getAPIVersion
  return $ "group_id" <:> groupId
    ++ "access_token" <:> token
    ++ "v" <:> version

longPoll :: Update.Init -> TimeOut -> Query
longPoll ini timeout =
  "act" <:> ("a_check" :: String)
    ++ "key" <:> Update.key ini
    ++ "ts" <:> Update.ts ini
    ++ "wait" <:> timeout

sendMessage :: (MError m, MCache m) => Update.Update -> [Label] -> m Query
sendMessage (cid, Update.Entity emc as) btns = do
  token <- Cache.getToken
  version <- Cache.getAPIVersion
  case emc of
    Right _ -> Error.throw $ Error.QueryError "Unable to send command to user"
    Left message -> do
      let qDefault = queryDefault token cid version
      let qMessage = "message" <:> message
      let qButtons = if null btns then [] else "keyboard" <:> Encode.keyboard btns
      let qAttachments = queryAttachments as
      return $ qDefault ++ qMessage ++ qButtons ++ qAttachments