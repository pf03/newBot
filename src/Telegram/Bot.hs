{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}

module Telegram.Bot (Pointer (..), reset) where

-- Our modules
import           Common.Misc
import           Interface.MLog           as Log
import           Interface.MT
import           Interface.MCache as Cache
import           Interface.Messenger.IBot
import           Interface.Messenger.IUpdate
import qualified Logic.Request as Request
import qualified Telegram.Parse           as Parse
import qualified Telegram.Query           as Query
import           Telegram.Update           as Update
import           Telegram.API           as API

--Other modules
import           Control.Applicative
import qualified System.Console.ANSI      as Color (Color (..))

-----------------------------Types---------------------------------------------
data Pointer = Pointer
-- New type wrappers in order to avoid orphan instances
newtype Init = Init UpdateId
newtype WrapUpdate = WrapUpdate Update deriving newtype (IUpdate)

-----------------------------Instance------------------------------------------
instance IBot Pointer Init WrapUpdate where
    getInit :: MT m => Pointer -> m Init
    getInit _ = Init <$> _getUpdateId 

    getUpdateId :: Init -> UpdateId
    getUpdateId (Init uid) = uid

    setUpdateId :: Init -> UpdateId -> Init
    setUpdateId _ newuid = Init newuid

    getUpdates:: MT m => Init -> m ([WrapUpdate], Init)
    getUpdates (Init uid) = do
       (us, Just newuid) <- _getUpdates . Just $ uid
       return (WrapUpdate <$> us, Init newuid)

    sendMessage:: MT m => WrapUpdate -> [Label] -> m ()
    sendMessage (WrapUpdate u) ls = _sendMessage u ls

--------------------------------Internal functions----------------------------------------
-- Initialization - get last updateId for getUpdates request
_getUpdateId :: MT m => m UpdateId
_getUpdateId = do
    Log.setSettings Color.Blue True "_getUpdateId"
    uidFromFile <- Cache.getUpdateIdFromFile
    if uidFromFile
        then  return 0  -- if we get updateId from a file, then initialization is not needed
        else do
            Log.send
            (_, muid) <- _getUpdates Nothing
            Log.receive
            maybe _getUpdateId (return . (-) 1 ) muid

-- Get updates from messenger server by the long polling method
-- _getUpdates Nothing - for initialization
-- _getUpdates (Just uid) - for get updates
_getUpdates :: MT m => Maybe UpdateId -> m ([Update], Maybe UpdateId)
_getUpdates muid = do
    Log.setSettings Color.Cyan True $ template "_getUpdates, muid = {0}" [show muid]
    Log.send
    response <- Request.api GetUpdates (Query.getUpdates (fmap (+1) muid) 25) True
    Log.receive
    o <- Parse.getObject response
    Log.receiveData "object -- convert" o
    mnewuid <- Parse.updateId o
    Log.receiveData "mnewuid" mnewuid
    us <- Parse.updates o
    Log.receiveData "update" us
    return (us, mnewuid <|> muid)

-- Send response to a single user
_sendMessage :: MT m => Update -> [Label] -> m ()
_sendMessage update btns = do
    Log.setSettings Color.Yellow True "sendMessage"
    Log.send
    (api, query) <- Query.sendMessage update btns
    Log.receiveData "(api, query)" (api, query)
    json <- Request.api api query False
    Log.receive
    o <- Parse.getObject json
    Log.receiveData "object" o

-- Dumping messages that we cannot parse, for debugging purposes
reset :: MT m => m ()
reset = do
    uid <- Cache.getUpdateId
    Log.setSettings Color.Cyan True $ template "reset, uid = {0}" [show uid]
    Log.send
    json <- Request.api GetUpdates (Query.getUpdates (Just uid) 0) True
    Log.receive
    o <- Parse.getObject json
    mnewuid <- Parse.updateId o
    Log.receiveData "mnewuid" mnewuid