{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

--importPriority = 59
module Telegram.App where

import qualified App
import Types --100
import Telegram.Types 

-- instance App.Main Pointer UpdateId Update where
--     getInit pointer = _getUpdateId
--     getUpdates pointer uid = do
--        (us, Just uid) <- _getUpdates . Just $ uid
--        return (us, uid)
--     sendMessage pointer = _sendMessage
--     setMessage = _setMessage
--     getMessage = _getMessage
--     getCommand = _getCommand

