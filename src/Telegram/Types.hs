--importPriority = 99.5
module Telegram.Types where
import qualified App

import Types  --100 

    ------------------------------------API-----------------------------------------------------------------
--можно сделать чтото типа 
--data API = GetUpdates | Send Entity
data API =  GetUpdates | SendMessage | SendSticker | SendAnimation | SendPhoto | SendVideo |SendDocument| SendPoll | SendContact | SendLocation
    | CopyMessage | ForwardMessage  deriving Show




data Entity = Message Message 
    | Command Command 
    | Sticker FileId  --StickerId String или Int для Telegram или VK
    | Animation FileId 
    | Photo FileId (Maybe Caption)
    | Video FileId (Maybe Caption) 
    | Document FileId (Maybe Caption) 
    | Poll {pollId :: StrId, question :: String, options :: [String]}
    | Contact {phoneNumber:: String, firstName::String, mlastName::Maybe String, mvCard::Maybe String}
    | Location Float Float
    | Forward ChatId IntId  --from_chat_id, message_id
    | Other IntId  --возможно этот тип покрывает все остальные, но я об этом не знал

    deriving Show 

data Pointer = Pointer --синглтон для указания на текущее приложение

--Update соответствует одному элементу корневого массива json
type Update = (ChatId, Entity) --deriving Show