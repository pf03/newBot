module Interface.MError.Functions where

import qualified Control.Exception as E
import Control.Monad.Except (ExceptT, MonadIO (..))
import Control.Monad.Trans.Except (catchE, throwE)
import Interface.MError.Class (MError (..), MIOError)
import Interface.MError.Types (E (Exit, IOError, SomeError), EE)

-----------------------------MError--------------------------------------------
liftE :: MError m => Either E a -> m a
liftE ea = case ea of
  Left e -> throw e
  Right a -> return a

catchEither :: MError m => Either b a -> (b -> E) -> m a
catchEither eba handler = case eba of
  Left b -> throw $ handler b
  Right a -> return a

toEither :: MError m => m a -> m (Either E a)
toEither ma = do
  catch (Right <$> ma) $ \e -> return $ Left e

-----------------------------MIOError------------------------------------------
catchEIO :: (MIOError m, E.Exception e) => IO a -> (e -> E) -> m a
catchEIO m h = do
  ea <- liftIO $ (Right <$> m) `E.catch` handler
  liftE ea
  where
    --handler :: Exception e => (e -> IO (EE a))
    handler e = return . Left . h $ e

-- * The same as previous, but errors are handled automatically, without user handlers

liftEIO :: MIOError m => IO a -> m a
liftEIO m = do
  ea <- liftIO $ (Right <$> m) `E.catch` ehandler `E.catch` iohandler `E.catch` otherhandler
  liftE ea
  where
    ehandler :: E.AsyncException -> IO (EE a)
    ehandler _ = return $ Left Exit
    iohandler :: E.IOException -> IO (EE a)
    iohandler e = return . Left . IOError . show $ e
    otherhandler :: E.SomeException -> IO (EE a)
    otherhandler e = return . Left . SomeError . show $ e