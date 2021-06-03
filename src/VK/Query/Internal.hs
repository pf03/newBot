module VK.Query.Internal where

import Common.Types( UserId, Message) 
import Common.Functions(template, safeTail) 
import Common.Convert((<:>), jconvert) 
import Data.Either (rights)
import Network.HTTP.Simple (Query, QueryItem)
import qualified VK.Encode as Encode (contentUrl)
import qualified VK.Update as Update
import qualified VK.API as API
import qualified Interface.MCache.Exports as Cache

defaultQuery :: Cache.Token -> UserId -> API.Version -> Query
defaultQuery token userId version = "peer_id" <:> userId ++ "access_token" <:> token ++ "v" <:> version

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
    Update.Sticker stikerId -> Right ("sticker_id", jconvert stikerId)
    Update.Link url -> Right ("content_source", jconvert $ Encode.contentUrl url)
    Update.Audio ownerId objectId -> Left $ template "audio{0}_{1}" [show ownerId, show objectId]
    Update.Wall ownerId objectId -> Left $ template "wall{0}_{1}" [show ownerId, show objectId]
    Update.Item itemName ownerId objectId accessKey ->
      Left $
        template "{3}{0}_{1}_{2}" [show ownerId, show objectId, accessKey, itemName]