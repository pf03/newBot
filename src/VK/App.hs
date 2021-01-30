{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
--importPriority = 59
module VK.App where

import qualified App  --60
import Types  --100
import VK.Types   --100.5




--updates можно сделать отдельным классом
-- instance App.Main Pointer Init Update  where
--     getInit pointer = _getInit 
--     getUpdates pointer = _getUpdates
--     sendMessage pointer = _sendMessage
--     setMessage = _setMessage
--     getMessage = _getMessage
--     getCommand = _getCommand