{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Telegram.Update where

-- Our modules
import           Common.Misc
import           Interface.Messenger.IUpdate as Update

-----------------------------Types---------------------------------------------

-- Update matches one element of the root json array
type Update = (ChatId, Entity)
type Caption = String
data Entity = Message Message
    | Command Command
    | Sticker FileId 
    | Animation FileId
    | Photo FileId (Maybe Caption)
    | Video FileId (Maybe Caption)
    | Document FileId (Maybe Caption)
    | Poll {pollId :: StrId, question :: String, options :: [String]}
    | Contact {
        phoneNumber:: String, 
        firstName::String, 
        mlastName :: Maybe String, 
        mvCard::Maybe String}
    | Location Float Float
    | Forward ChatId IntId
    | Other IntId 
    deriving Show

--------------------------instance App.Update----------------------------------
instance IUpdate Update  where
    setMessage :: Update -> Message -> Update
    setMessage (cid, Message _) message = (cid, Message message)
    setMessage (cid, Photo fileId (Just _) ) message = (cid, Photo fileId (Just message))
    setMessage (cid, Video fileId (Just _) ) message = (cid, Video fileId (Just message))
    setMessage (cid, Command _ ) message = (cid, Message message)
    setMessage u _ = u

    getMessage :: Update -> Maybe Message
    getMessage (_, Message message)         = Just message
    getMessage (_, Photo _ (Just message) ) = Just message
    getMessage (_, Video _ (Just message) ) = Just message
    getMessage _                            = Nothing

    getCommand :: Update -> Maybe Command
    getCommand (_, Command command) = Just command
    getCommand _                    = Nothing

    getChatId :: Update -> ChatId
    getChatId = fst