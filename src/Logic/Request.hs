{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
--importPriority = 10
module Logic.Request (build, send, api) where 

-- Our modules
import Interface.Error as Error --70
-- import Types --100
-- import App
import Common.Misc
-- import Class
import Interface.Cache as Cache
import Interface.App as App
-- import Interface.Class

-- Other modules
import Control.Monad.Trans.Except
import Control.Monad ( when )
import Network.HTTP.Simple
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Char ( toLower )
--import Debug.Trace  --unsafe debug!!!
import Control.Monad.State.Lazy
import Control.Monad.Error.Class
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
send :: MIOError m => Request -> Bool -> m LBS
send request save = do
  --printT request
  response <- httpLBS request --- ЗДЕСЬ ПРИКРУТИТЬ ОБРАБОТКУ ОШИБОК !!!!
  let status = getResponseStatusCode response
  if status == 200
  then do
    let jsonBody = getResponseBody response
    when save $ do 
      liftIO $ print "saving request to file"
      Error.liftEIO  $ L.writeFile "data.json" jsonBody 
    return  jsonBody
  else do 
    printT "Request failed with error"
    printT response
    Error.throw $ QueryError "Request failed with error"

--высокоуровневая обертка, как оказалось для Telegram подходит отлично, а для VK не подходит, верней не подходит только для longPolling
api :: (API api, MCache m, MIOError m) => api -> Query -> Bool -> m LBS
api api query save = do
  --config@(Config _ configApp _) <- get
  host <- Cache.getHost
  token <- Cache.getToken
  let path = getPath token api
  let request = build host path query
  --printT request
  send request save
















