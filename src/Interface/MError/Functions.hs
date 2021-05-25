module Interface.MError.Functions where

import qualified Control.Exception as E
import Control.Monad.Except ( MonadIO(liftIO) ) 
import Interface.MError.Class ( MError(..), MIOError ) 
import Interface.MError.Types ( Error(SomeError, Exit, IOError) )
import Control.Concurrent.Async ( AsyncCancelled )

-----------------------------MError--------------------------------------------
liftE :: MError m => Either Error a -> m a
liftE ea = case ea of
  Left e -> throw e
  Right a -> return a

catchEither :: MError m => Either b a -> (b -> Error) -> m a
catchEither eba handler = case eba of
  Left b -> throw $ handler b
  Right a -> return a

toEither :: MError m => m a -> m (Either Error a)
toEither ma = do
  catch (Right <$> ma) $ \e -> return $ Left e

-----------------------------MIOError------------------------------------------
catchEIO :: (MIOError m, E.Exception e) => IO a -> (e -> Error) -> m a
catchEIO m h = do
  ea <- liftIO $ (Right <$> m) `E.catch` handler
  liftE ea
  where
    --handler :: Exception e => (e -> IO (EE a))
    handler e = return . Left . h $ e

-- * The same as previous, but errors are handled automatically, without user handlers

liftEIO :: MIOError m => IO a -> m a
liftEIO m = do
  ea <- liftIO $ (Right <$> m) `E.catch` asynchandler `E.catch` ehandler `E.catch` iohandler `E.catch` otherhandler
  liftE ea
  where
    asynchandler :: AsyncCancelled -> IO (Either Error a)
    asynchandler _ = return $ Left Exit
    ehandler :: E.AsyncException -> IO (Either Error a)
    ehandler _ = return $ Left Exit
    iohandler :: E.IOException -> IO (Either Error a)
    iohandler e = return . Left . IOError . show $ e
    otherhandler :: E.SomeException -> IO (Either Error a)
    otherhandler e = return . Left . SomeError . show $ e