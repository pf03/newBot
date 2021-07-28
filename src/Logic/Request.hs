{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Logic.Request where

import Class
import Common.Types (Host (..), Path (..))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, throw)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Lazy (when)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Interface.Error.Exports as Error
import qualified Interface.Log.Exports as Log
import qualified Network.HTTP.Simple as HTTP

-- | Low level wrapper for request
sendRequest :: (MLog m, MonadIO m) => HTTP.Request -> Bool -> m LC.ByteString
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
      throw $ Error.QueryError "Request failed with error"
  where
    getResponse :: (MLog m, MonadIO m) => m (HTTP.Response LC.ByteString)
    getResponse = do
      eResponse <- Error.try (HTTP.httpLBS request)
      case eResponse of
        Left Error.Exit -> do
          -- Exit from application by user choice
          throw Error.Exit
        Left _ -> do
          Log.writeErrorM "Network connection error. Timeout 3 sec..."
          Log.writeErrorM $ show eResponse
          Error.liftEIO $ threadDelay 3000000 -- liftEIO for correct catch async exceptions
          getResponse
        Right response -> do
          Log.writeInfoM "Response received successfully"
          return response

buildRequest :: Host -> Path -> HTTP.Query -> HTTP.Request
buildRequest (Host host) (Path path) query =
  HTTP.setRequestHost (BC.pack host) $
    HTTP.setRequestPath (BC.pack path) $
      HTTP.setRequestBodyURLEncoded
        (map (\(a, Just b) -> (a, b)) query)
        $ setRequestSettings HTTP.defaultRequest

parseRequest :: String -> HTTP.Query -> HTTP.Request
parseRequest str query = request
  where
    eRequest = HTTP.parseRequest str :: Either SomeException HTTP.Request
    initRequest = case eRequest of
      Left err -> throw $ Error.QueryError (show err)
      Right request0 -> request0
    request =
      HTTP.setRequestBodyURLEncoded
        (map (\(a, Just b) -> (a, b)) query)
        $ setRequestSettings initRequest

setRequestSettings :: HTTP.Request -> HTTP.Request
setRequestSettings initRequest =
  HTTP.setRequestSecure True $
    HTTP.setRequestMethod "POST" $
      HTTP.setRequestPort
        443
        initRequest