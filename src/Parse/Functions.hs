{-# LANGUAGE FlexibleInstances #-}

module Parse.Functions where

import Common.Types (LBS)
import Data.Aeson (FromJSON, Object, Value, eitherDecode)
import Class (MError)
import qualified Interface.Error.Exports as Error

getObject :: MError m => LBS -> m Object
getObject bs = Error.catchEither (eitherDecode bs) Error.ParseError

getValue :: MError m => LBS -> m Value
getValue bs = Error.catchEither (eitherDecode bs) Error.ParseError

eDecode :: (MError m, FromJSON a) => LBS -> m a
eDecode bs = Error.catchEither (eitherDecode bs) Error.ParseError
