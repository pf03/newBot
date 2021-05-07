--{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

--importPriority = 50
module Logic.Parse where

-- Our modules
import Interface.Error as Error  --70
import Common.Misc

-- Other modules
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Text (Text, pack)
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Simple
import Data.Maybe

-----------------------------Types---------------------------------------------


-----------------From JSON------------------------------

getObject :: MError m => LBS -> m Object
getObject bs = catchEither (eitherDecode bs) ParseError

getValue :: MError m => LBS -> m Value 
getValue bs = catchEither (eitherDecode bs) ParseError

eDecode :: (MError m, FromJSON a) => LBS -> m a
eDecode bs = catchEither (eitherDecode bs) ParseError

--выход из монады Parser
_parseE :: MError m => (Object -> Parser a) -> Object -> m a
_parseE f o = catchEither (parseEither f o) ParseError

_parseJSONo :: FromJSON a => Object -> Parser a
_parseJSONo = parseJSON . Object

--обертка для работы с опциональным полем 
_mwithItem :: Key -> (Object -> Parser  a) -> Object -> Parser (Maybe a)
_mwithItem k f o = do
    mo <- o .:? pack k
    case mo of
        Nothing -> return Nothing
        Just o1 -> Just <$> f o1  

--обертка для работы с внутренними массивами
_withArrayItem :: Key -> (Object -> Parser a) -> Object -> Parser [a]
_withArrayItem k f o = do
    arr <- o .: pack k
    mapM f arr

--обертка для работы с внутренними опциональными массивами
_mwithArrayItem :: Key -> (Object -> Parser a) -> Object -> Parser (Maybe [a])
_mwithArrayItem k f o = do
    marr <- o .:? pack k -- :: Parser (Maybe [Object])
    case marr of
         Nothing -> return Nothing
         Just arr -> Just <$> mapM f arr

--обертка для работы с внутренними массивами с опциональными элементами
_withArraymItem :: Key -> (Object -> Parser (Maybe a)) -> Object -> Parser [a]
_withArraymItem k f o = do
    ma <- _withArrayItem k f o
    return $ fmap fromJust . filter isJust $ ma

----------------------------------------To JSON-------------------------------------

(<:>) :: Convert a => String -> a -> Query
(<:>) key value = [(convert key, jc value)]
infixr 7 <:>

(<:?>) :: Convert a => String -> Maybe a -> Query
(<:?>) key mvalue = case mvalue of
  Nothing -> []
  Just value -> [(convert key, jc value)]
infixr 7 <:?>
