{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Logic.Request (build, send, api) where

import Common.Misc (LBS, Path)
import Control.Concurrent (threadDelay)
import Control.Monad.State.Lazy (when)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Interface.MCache as Cache (Host, MCache)
import qualified Interface.MCache as Cache
import Interface.MError as Error (E (Exit, QueryError), MIOError)
import qualified Interface.MError as Error
import qualified Interface.MLog.Exports as Log
import Interface.Messenger.IAPI as API (IAPI (getPath))
import Network.HTTP.Simple as HTTP (Query, Request, Response)
import qualified Network.HTTP.Simple as HTTP

build :: Host -> Path -> Query -> Request
build h path query =
  HTTP.setRequestSecure True $
    HTTP.setRequestMethod "POST" $
      HTTP.setRequestPort 443 $
        HTTP.setRequestHost (BC.pack h) $
          HTTP.setRequestPath (BC.pack path) $
            HTTP.setRequestBodyURLEncoded
              (map (\(a, Just b) -> (a, b)) query)
              HTTP.defaultRequest

-- | Low level wrapper for request
send :: (Log.MLog m, MIOError m) => Request -> Bool -> m LBS
send request save = do
  -- Log.debugM request
  response <- resp
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
      Error.throw $ QueryError "Request failed with error"
  where
    resp :: (Log.MLog m, MIOError m) => m (Response LBS)
    resp = do
      er <- Error.toEither $ Error.liftEIO (HTTP.httpLBS request)
      case er of
        Left Error.Exit -> do
          -- Exit from application by user choise
          Error.throw Error.Exit
        Left e -> do
          Log.errorM "Network connection error. Timeout 3 sec..."
          Log.errorM $ show e
          Error.liftEIO $ threadDelay 3000000 -- liftEIO for correct catch async exceptions
          resp
        Right r -> return r

-- | High level wrapper for API request
api :: (IAPI api, MCache m, MIOError m, Log.MLog m) => api -> Query -> Bool -> m LBS
api a query save = do
  host <- Cache.getHost
  token <- Cache.getToken
  let path = API.getPath token a
  let request = build host path query
  send request save
