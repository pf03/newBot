{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Class where

import Control.Monad.Trans.Except
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Lazy

import Data.Text (pack)
import Data.Text.Encoding
import Network.HTTP.Simple
import Data.Aeson.Types
import Data.Aeson

import Types

--import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
--import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
----------------------------------ToTransformer--------------------------------------------
--подъем до основного трасформера
class ToTransformer m where 
    toT :: m a -> T a

instance ToTransformer T where
    toT = id

instance ToTransformer (Except E) where
    toT = lift . except . runExcept 

instance ToTransformer (ExceptT E IO) where
    toT = lift

instance ToTransformer IO where
    toT = liftIO

instance ToTransformer (Reader S) where
    toT reader = do
        s <- get
        let res = runReader reader s
        return res

instance ToTransformer (State S) where
    toT state = do
        s <- get
        let a = evalState state s
        return a


----------------------------------------Convert--------------------------------------------------
class Convert a where
    convert :: a -> BC.ByteString

instance Convert String where
    convert = encodeUtf8 . pack  --encodeUtf8 для корректной кодировки кирилицы

instance Convert LC.ByteString where
  convert = BC.pack . LC.unpack

instance Convert Int where
  convert = BC.pack . show

instance Convert Float where
  convert = BC.pack . show

instance Convert Value where 
     convert = convert . encode 

instance Convert Object where 
     convert = convert . encode 

jc :: Convert a => a -> Maybe BC.ByteString
jc = Just . convert