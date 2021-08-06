module Logic.VK.Query.Functions where

import Common.Convert ((<:>))
import Common.Types (Label, TimeOut, Token (Token))
import qualified Interface.Cache.Exports as Cache
import qualified Interface.Error.Exports as Error
import qualified Logic.VK.Encode as Encode
import qualified Logic.VK.Query.Internal as Internal
import qualified Messenger.Update.VK.Types as Update
import Network.HTTP.Simple (Query)

getLongPollServerQuery :: Cache.MCache m => m Query
getLongPollServerQuery = do
  groupId <- Cache.getGroupId
  Token token <- Cache.getToken
  version <- Cache.getAPIVersion
  return $
    "group_id" <:> groupId
      ++ "access_token" <:> token
      ++ "v" <:> version

longPollQuery :: Update.Init -> TimeOut -> Query
longPollQuery ini timeout =
  "act" <:> ("a_check" :: String)
    ++ "key" <:> Update.key ini
    ++ "ts" <:> Update.ts ini
    ++ "wait" <:> timeout

sendMessageQuery :: Cache.MCache m => Update.Update -> [Label] -> m (Either Error.Error Query)
sendMessageQuery (chatId, Update.Entity eMessageCommand attachments) btns = do
  token <- Cache.getToken
  version <- Cache.getAPIVersion
  case eMessageCommand of
    Right _ -> return $ Left $ Error.QueryError "Unable to send command to user"
    Left message -> do
      let defaultQuery = Internal.defaultQuery token chatId version
      let messageQuery = "message" <:> message
      let buttonsQuery = if null btns then [] else "keyboard" <:> Encode.encodeKeyboard btns
      let attachmentsQuery = Internal.attachmentsQuery attachments
      return $ Right $ defaultQuery ++ messageQuery ++ buttonsQuery ++ attachmentsQuery