{-# LANGUAGE FlexibleInstances #-}

module Parse.Functions where

import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Aeson (FromJSON, Object, Value, eitherDecode)
import Class (MError)
import qualified Interface.Error.Exports as Error

getObject :: MError m => LC.ByteString -> m Object
getObject bs = Error.catchEither (eitherDecode bs) Error.ParseError

getValue :: MError m => LC.ByteString -> m Value
getValue bs = Error.catchEither (eitherDecode bs) Error.ParseError

eDecode :: (MError m, FromJSON a) => LC.ByteString -> m a
eDecode bs = Error.catchEither (eitherDecode bs) Error.ParseError
