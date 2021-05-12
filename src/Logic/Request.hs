{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Logic.Request (build, send, api) where

-- Our modules
import           Common.Misc
import           Interface.MCache         as Cache hiding (host, token)
import           Interface.MError         as Error
import           Interface.MLog           as Log hiding (send)
import           Interface.Messenger.IAPI as API

-- Other modules
import           Control.Monad.State.Lazy
import qualified Data.ByteString.Char8    as BC
import qualified Data.ByteString.Lazy     as L
import           Network.HTTP.Simple

build :: Host -> Path -> Query -> Request
build h path query = setRequestSecure True
    $ setRequestMethod "POST"
    $ setRequestPort 443
    $ setRequestHost (BC.pack h)
    $ setRequestPath (BC.pack path)
    $ setRequestBodyURLEncoded (map (\(a, Just b) -> (a, b) ) query)
    defaultRequest

-- | Low level wrapper for request
send :: (MLog m, MIOError m) => Request -> Bool -> m LBS
send request save = do
    Log.debugM request


    response <- Error.liftEIO $ httpLBS request
    let status = getResponseStatusCode response
    if status == 200
    then do
        response <- req 1
        let jsonBody = getResponseBody response
        when save $ do
            Log.warnM "Saving request to file"
            Error.liftEIO  $ L.writeFile "data.json" jsonBody
        return  jsonBody
    else do
        Log.errorM "Request failed with error"
        Log.errorM $ show response
        Error.throw $ QueryError "Request failed with error" where

            req :: (MLog m, MIOError m) => Int -> m (Response LBS)
            req n = Error.catch (Error.liftEIO $ httpLBS request) $ \e -> do
                Log.errorM "Request failed. Attempt 1 of 3"
                if n >= 3 then Error.throw e else req (n+1) 


-- | High level wrapper for API request
api :: (IAPI api, MCache m, MIOError m, MLog m) => api -> Query -> Bool -> m LBS
api a query save = do
  host <- Cache.getHost
  token <- Cache.getToken
  let path = API.getPath token a
  let request = build host path query
  send request save
