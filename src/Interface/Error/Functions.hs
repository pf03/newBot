module Interface.Error.Functions where

import Common.Functions (template)
import Control.Concurrent.Async (AsyncCancelled)
import qualified Control.Exception as E
import Control.Monad.Except (MonadIO (liftIO))
-- import Interface.Error.Class (MError (..))
import Interface.Error.Types (Error (ConfigError, Exit, IOError, SomeError))

-----------------------------MError--------------------------------------------
-- liftE :: MError m => Either Error a -> m a
-- liftE ea = case ea of
--   Left err -> throw err
--   Right a -> return a

catchEither :: Monad m => Either b a -> (b -> Error) -> m a
catchEither eba handler = case eba of
  Left b -> E.throw $ handler b
  Right a -> return a

-- это аналог try
-- toEither :: MError m => m a -> m (Either Error a)
-- toEither ma = do
--   catch (Right <$> ma) $ \err -> return $ Left err

-----------------------------MIOError------------------------------------------
catchEIO :: (MonadIO m, E.Exception e) => IO a -> (e -> Error) -> m a
catchEIO m h = do
  ea <- liftIO $ (Right <$> m) `E.catch` handler
  liftE ea
  where
    --handler :: Exception e => (e -> IO (EE a))
    handler err = return . Left . h $ err

-- * The same as previous, but errors are handled automatically, without user handlers

-- liftEIO :: (MonadIO m, MError m) => IO a -> m a
-- liftEIO m = do
--   ea <- liftIO $ (Right <$> m) `E.catch` asyncHandler `E.catch` eHandler `E.catch` ioHandler `E.catch` otherHandler
--   liftE ea
--   where
--     asyncHandler :: AsyncCancelled -> IO (Either Error a)
--     asyncHandler _ = return $ Left Exit
--     eHandler :: E.AsyncException -> IO (Either Error a)
--     eHandler _ = return $ Left Exit
--     ioHandler :: E.IOException -> IO (Either Error a)
--     ioHandler err = return . Left . IOError . show $ err
--     otherHandler :: E.SomeException -> IO (Either Error a)
--     otherHandler err = return . Left . SomeError . show $ err

liftEIO :: (MonadIO m) => IO a -> m a
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

throwConfig :: (Monad m) => String -> [String] -> m a
throwConfig str args = E.throw $ ConfigError $ template str args

liftE :: Monad m => Either Error a -> m a
liftE ea = case ea of
  Left err -> E.throw err
  Right a -> return a

try :: (MonadIO m) => IO a -> m (Either Error a)
try ma = liftIO $ (Right <$> ma) `E.catch` asyncHandler `E.catch` eHandler `E.catch` ioHandler `E.catch` otherHandler where
    asyncHandler :: AsyncCancelled -> IO (Either Error a)
    asyncHandler _ = return $ Left Exit
    eHandler :: E.AsyncException -> IO (Either Error a)
    eHandler _ = return $ Left Exit
    ioHandler :: E.IOException -> IO (Either Error a)
    ioHandler err = return . Left . IOError . show $ err
    otherHandler :: E.SomeException -> IO (Either Error a)
    otherHandler err = return . Left . SomeError . show $ err
