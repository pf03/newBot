module Interface.Error.Functions where

import Common.Functions (template)
import Control.Concurrent.Async (AsyncCancelled)
import qualified Control.Exception as E
import Control.Monad.Except (MonadIO (liftIO))
import Interface.Error.Class (MError (..), MIOError)
import Interface.Error.Types (Error (ConfigError, Exit, IOError, SomeError))

-----------------------------MError--------------------------------------------
liftE :: MError m => Either Error a -> m a
liftE ea = case ea of
  Left err -> throw err
  Right a -> return a

catchEither :: MError m => Either b a -> (b -> Error) -> m a
catchEither eba handler = case eba of
  Left b -> throw $ handler b
  Right a -> return a

toEither :: MError m => m a -> m (Either Error a)
toEither ma = do
  catch (Right <$> ma) $ \err -> return $ Left err

-----------------------------MIOError------------------------------------------
catchEIO :: (MIOError m, E.Exception e) => IO a -> (e -> Error) -> m a
catchEIO m h = do
  ea <- liftIO $ (Right <$> m) `E.catch` handler
  liftE ea
  where
    --handler :: Exception e => (e -> IO (EE a))
    handler err = return . Left . h $ err

-- * The same as previous, but errors are handled automatically, without user handlers

liftEIO :: MIOError m => IO a -> m a
liftEIO m = do
  ea <- liftIO $ (Right <$> m) `E.catch` asyncHandler `E.catch` eHandler `E.catch` ioHandler `E.catch` otherHandler
  liftE ea
  where
    asyncHandler :: AsyncCancelled -> IO (Either Error a)
    asyncHandler _ = return $ Left Exit
    eHandler :: E.AsyncException -> IO (Either Error a)
    eHandler _ = return $ Left Exit
    ioHandler :: E.IOException -> IO (Either Error a)
    ioHandler err = return . Left . IOError . show $ err
    otherHandler :: E.SomeException -> IO (Either Error a)
    otherHandler err = return . Left . SomeError . show $ err

throwConfig :: (MError m) => String -> [String] -> m a
throwConfig str args = throw $ ConfigError $ template str args