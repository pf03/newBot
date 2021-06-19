{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Logic.Request where

import Common.Convert ( LBS )
import Common.Types ( Host(..), Path(..) )
import Control.Concurrent (threadDelay)
import Control.Monad.State.Lazy (when)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Class (IAPI, MCache, MIOError, MLog)
import qualified Interface.Cache.Exports as Cache
import qualified Interface.Error.Exports as Error
import qualified Interface.Log.Exports as Log
import qualified Messenger.API.Class as API
import qualified Network.HTTP.Simple as HTTP

-- | Low level wrapper for request
sendRequest :: (MLog m, MIOError m) => HTTP.Request -> Bool -> m LBS
sendRequest request save = do
  response <- getResponse
  let status = HTTP.getResponseStatusCode response
  if status == 200
    then do
      let jsonBody = HTTP.getResponseBody response
      Log.debugM jsonBody
      when save $ do
        Log.warnM "Saving request to file"
        Error.liftEIO $ L.writeFile "data.json" jsonBody
      return jsonBody
    else do
      Log.errorM "Request failed with error"
      Log.errorM $ show response
      Error.throw $ Error.QueryError "Request failed with error"
  where
    getResponse :: (MLog m, MIOError m) => m (HTTP.Response LBS)
    getResponse = do
      err <- Error.toEither $ Error.liftEIO (HTTP.httpLBS request)
      case err of
        Left Error.Exit -> do
          -- Exit from application by user choise
          Error.throw Error.Exit
        Left _ -> do
          Log.errorM "Network connection error. Timeout 3 sec..."
          Log.errorM $ show err
          Error.liftEIO $ threadDelay 3000000 -- liftEIO for correct catch async exceptions
          getResponse
        Right response -> return response

-- | High level wrapper for API request
sendApiRequest :: (IAPI api, MCache m, MIOError m, MLog m) => api -> HTTP.Query -> Bool -> m LBS
sendApiRequest api query save = do
  host <- Cache.getHost
  token <- Cache.getToken
  let path = API.getPath token api
  let request = buildRequest host path query
  sendRequest request save

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