{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Logic.Request where

import Class (IAPI, MCache, MError, MIOError, MLog)
import Common.Types (Host (..), Path (..))
import Control.Concurrent (threadDelay)
import Control.Monad.Catch (SomeException)
import Control.Monad.State.Lazy (when)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Interface.Cache.Exports as Cache
import qualified Interface.Error.Exports as Error
import qualified Interface.Log.Exports as Log
import qualified Messenger.API.Class as API
import qualified Network.HTTP.Simple as HTTP

-- | Low level wrapper for request
sendRequest :: (MLog m, MIOError m) => HTTP.Request -> Bool -> m LC.ByteString
sendRequest request save = do
  response <- getResponse
  let status = HTTP.getResponseStatusCode response
  if status == 200
    then do
      let jsonBody = HTTP.getResponseBody response
      Log.writeDebugM jsonBody
      when save $ do
        Log.writeWarnM "Saving request to file"
        Error.liftEIO $ L.writeFile "data.json" jsonBody
      return jsonBody
    else do
      Log.writeErrorM "Request failed with error"
      Log.writeErrorM $ show response
      Error.throw $ Error.QueryError "Request failed with error"
  where
    getResponse :: (MLog m, MIOError m) => m (HTTP.Response LC.ByteString)
    getResponse = do
      err <- Error.toEither $ Error.liftEIO (HTTP.httpLBS request)
      case err of
        Left Error.Exit -> do
          -- Exit from application by user choice
          Error.throw Error.Exit
        Left _ -> do
          Log.writeErrorM "Network connection error. Timeout 3 sec..."
          Log.writeErrorM $ show err
          Error.liftEIO $ threadDelay 3000000 -- liftEIO for correct catch async exceptions
          getResponse
        Right response -> return response

-- | High level wrapper for API request
sendApiRequest :: (IAPI api, MCache m, MIOError m, MLog m) => api -> HTTP.Query -> Bool -> m LC.ByteString
sendApiRequest api query save = do
  host <- Cache.getHost
  token <- Cache.getToken
  let path = API.getPath token api
  let request = buildRequest host path query
  sendRequest request save

buildRequestWithInit :: HTTP.Request -> HTTP.Query -> HTTP.Request
buildRequestWithInit initRequest query =
  HTTP.setRequestSecure True $
    HTTP.setRequestMethod "POST" $
      HTTP.setRequestPort 443 $
        HTTP.setRequestBodyURLEncoded
          (map (\(a, Just b) -> (a, b)) query)
          initRequest

buildRequest :: Host -> Path -> HTTP.Query -> HTTP.Request
buildRequest (Host host) (Path path) query =
  HTTP.setRequestSecure True $
    HTTP.setRequestMethod "POST" $
      HTTP.setRequestPort 443 $
        HTTP.setRequestHost (BC.pack host) $
          HTTP.setRequestPath (BC.pack path) $
            HTTP.setRequestBodyURLEncoded
              (map (\(a, Just b) -> (a, b)) query)
              HTTP.defaultRequest

parseRequest :: MError m => String -> m HTTP.Request
parseRequest str = do
  let eRequest = HTTP.parseRequest str :: Either SomeException HTTP.Request
  case eRequest of
    Left err -> Error.throw $ Error.QueryError (show err)
    Right request -> return request