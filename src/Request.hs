{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
--importPriority = 10
module Request (build, send, api) where 

--наши модули
import Error --70
import Types --100

import Control.Monad.Trans.Except
import Control.Monad ( when )
import Network.HTTP.Simple
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Char ( toLower )
import Common
import Class


--import Debug.Trace  --unsafe debug!!!
import Control.Monad.State.Lazy
import Control.Monad.Error.Class
import App

import Data.Aeson

build :: Host -> Path -> Query -> Request
build host path query = setRequestSecure True
  $ setRequestMethod "POST"
  $ setRequestPort 443
  $ setRequestHost (BC.pack host)
  $ setRequestPath (BC.pack path)
  $ setRequestBodyURLEncoded (map (\(a, Just b) -> (a, b) ) query) --переделать покрасивее
  defaultRequest

--низкоуровневая обертка, нужна для VK
--работает ли здесь вообще обработка ошибок???
--сделать обработку ошибки записи как в writeConfig ?? либо какие-то универсальные функции для чтения и записи файлов с обработчиком ошибок
send :: Request -> Bool -> ExceptT E IO LC.ByteString
send request save = do
  --printT request
  response <- httpLBS request
  let status = getResponseStatusCode response
  if status == 200
  then do
    let jsonBody = getResponseBody response
    when save $ do 
      liftIO $ print "saving request to file"
      ExceptT $ toEE  $ L.writeFile "data.json" jsonBody 
    return  jsonBody
  else do 
    printT "Request failed with error"
    printT response
    throwError $ QueryError "Request failed with error"

--высокоуровневая обертка, как оказалось для Telegram подходит отлично, а для VK не подходит, верней не подходит только для longPolling
api :: (API api) => api -> Query -> Bool -> T LC.ByteString
api api query save = do
  --config@(Config _ configApp _) <- get
  host <- gets $ host . configApp
  token <- gets $ token . configApp
  let path = getPath token api
  let request = Request.build host path query
  --printT request
  toT $ Request.send request save
















