module Logic.VK.Query.Functions where

import qualified Logic.VK.Query.Internal as Internal
import Common.Types ( Token(Token), TimeOut, Label ) 
import Common.Convert((<:>)) 
import Class (MError, MCache)
import qualified Interface.Cache.Exports as Cache
import qualified Interface.Error.Exports as Error
import Network.HTTP.Simple (Query)
import qualified Logic.VK.Encode as Encode (keyboard)
import qualified Messenger.Update.VK.Types as Update

getLongPollServerQuery :: MCache m => m Query
getLongPollServerQuery = do
  groupId <- Cache.getGroupId
  Token token <- Cache.getToken
  version <- Cache.getAPIVersion
  return $ "group_id" <:> groupId
    ++ "access_token" <:> token
    ++ "v" <:> version

longPollQuery :: Update.Init -> TimeOut -> Query
longPollQuery ini timeout =
  "act" <:> ("a_check" :: String)
    ++ "key" <:> Update.key ini
    ++ "ts" <:> Update.ts ini
    ++ "wait" <:> timeout

sendMessageQuery :: (MError m, MCache m) => Update.Update -> [Label] -> m Query
sendMessageQuery (chatId, Update.Entity eMessageCommand attachments) btns = do
  token <- Cache.getToken
  version <- Cache.getAPIVersion
  case eMessageCommand of
    Right _ -> Error.throw $ Error.QueryError "Unable to send command to user"
    Left message -> do
      let defaultQuery = Internal.defaultQuery token chatId version
      let messageQuery = "message" <:> message
      let buttonsQuery = if null btns then [] else "keyboard" <:> Encode.keyboard btns
      let attachmentsQuery = Internal.attachmentsQuery attachments
      return $ defaultQuery ++ messageQuery ++ buttonsQuery ++ attachmentsQuery