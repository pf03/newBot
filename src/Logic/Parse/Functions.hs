{-# LANGUAGE FlexibleInstances #-}

module Logic.Parse.Functions where

import Common.Misc (LBS)
import Data.Aeson (FromJSON, Object, Value, eitherDecode)
import Interface.Class (MError)
import qualified Interface.MError.Exports as Error

getObject :: MError m => LBS -> m Object
getObject bs = Error.catchEither (eitherDecode bs) Error.ParseError

getValue :: MError m => LBS -> m Value
getValue bs = Error.catchEither (eitherDecode bs) Error.ParseError

eDecode :: (MError m, FromJSON a) => LBS -> m a
eDecode bs = Error.catchEither (eitherDecode bs) Error.ParseError