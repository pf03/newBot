module Parse.Functions where

import Data.Aeson (FromJSON, Object, Value, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Interface.Error.Exports as Error

getObject :: Monad m => LC.ByteString -> m Object
getObject bs = Error.catchEither (eitherDecode bs) Error.ParseError

getValue :: Monad m => LC.ByteString -> m Value
getValue bs = Error.catchEither (eitherDecode bs) Error.ParseError

eDecode :: (Monad m, FromJSON a) => LC.ByteString -> m a
eDecode bs = Error.catchEither (eitherDecode bs) Error.ParseError
