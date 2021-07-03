module Logic.VK.Query.Internal where

import Common.Convert (jConvert, (<:>))
import Common.Functions (safeTail, template)
import Common.Types (ChatId, ItemName (ItemName), Key (Key), Message, Token (..))
import Data.Either (rights)
import qualified Logic.VK.Encode as Encode
import qualified Messenger.API.VK.Types as API
import qualified Messenger.Update.VK.Types as Update
import Network.HTTP.Simple (Query, QueryItem)

defaultQuery :: Token -> ChatId -> API.Version -> Query
defaultQuery (Token token) userId version = "peer_id" <:> userId ++ "access_token" <:> token ++ "v" <:> version

messageQuery :: Message -> Query
messageQuery message = "message" <:> message

attachmentsQuery :: [Update.Attachment] -> Query
attachmentsQuery attachments = rights eStrQueryItem ++ queryStr
  where
    queryStr = if null str then [] else "attachment" <:> str
    str = safeTail $ foldl helper "" eStrQueryItem
    eStrQueryItem = map attachmentQuery attachments
    helper :: String -> Either String QueryItem -> String
    helper value1 (Left value2) = template "{0},{1}" [value1, value2]
    helper value _ = value

attachmentQuery :: Update.Attachment -> Either String QueryItem
attachmentQuery attachment =
  case attachment of
    Update.Sticker stickerId -> Right ("sticker_id", jConvert stickerId)
    Update.Link url -> Right ("content_source", jConvert $ Encode.encodeContentUrl url)
    Update.Audio ownerId objectId -> Left $ template "audio{0}_{1}" [show ownerId, show objectId]
    Update.Wall ownerId objectId -> Left $ template "wall{0}_{1}" [show ownerId, show objectId]
    Update.Item (ItemName itemName) ownerId objectId (Key accessKey) ->
      Left $
        template "{3}{0}_{1}_{2}" [show ownerId, show objectId, accessKey, itemName]