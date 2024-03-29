module Interface.Error.Functions where

import Common.Functions (template)
import Control.Concurrent.Async (AsyncCancelled)
import qualified Control.Exception as E
import Control.Monad.Except (MonadIO (liftIO))
import Interface.Error.Types (Error (ConfigError, Exit, IOError, SomeError))

catchEither :: Monad m => Either b a -> (b -> Error) -> m a
catchEither eba handler = case eba of
  Left b -> E.throw $ handler b
  Right a -> return a

catchEIO :: (MonadIO m, E.Exception e) => IO a -> (e -> Error) -> m a
catchEIO m h = do
  ea <- liftIO $ (Right <$> m) `E.catch` handler
  liftE ea
  where
    handler err = return . Left . h $ err

-- * The same as previous, but errors are handled automatically, without user handlers
liftEIO :: (MonadIO m) => IO a -> m a
liftEIO m = do
  ea <- try m
  liftE ea

throwConfig :: (MonadIO m) => String -> [String] -> m a
throwConfig str args = liftIO $ E.throwIO $ ConfigError $ template str args

liftE :: MonadIO m => Either Error a -> m a
liftE ea = case ea of
  Left err -> liftIO $ E.throwIO err
  Right a -> return a

try :: (MonadIO m) => IO a -> m (Either Error a)
try ma = liftIO $ (Right <$> ma) `E.catches` [E.Handler asyncHandler, E.Handler eHandler, E.Handler ioHandler, E.Handler otherHandler]
  where
    asyncHandler :: AsyncCancelled -> IO (Either Error a)
    asyncHandler _ = return $ Left Exit
    eHandler :: E.AsyncException -> IO (Either Error a)
    eHandler _ = return $ Left Exit
    ioHandler :: E.IOException -> IO (Either Error a)
    ioHandler err = return . Left . IOError . show $ err
    otherHandler :: E.SomeException -> IO (Either Error a)
    otherHandler err = return . Left . SomeError . show $ err