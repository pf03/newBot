module Interface.Error.Functions where

import Common.Functions ( template )
import qualified Control.Exception as E
import Control.Monad.Except ( MonadIO(liftIO) ) 
import Interface.Error.Class ( MError(..), MIOError ) 
import Interface.Error.Types ( Error(ConfigError, Exit, IOError, SomeError) )
import Control.Concurrent.Async ( AsyncCancelled )

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
  ea <- liftIO $ (Right <$> m) `E.catch` asynchandler `E.catch` ehandler `E.catch` iohandler `E.catch` otherhandler
  liftE ea
  where
    asynchandler :: AsyncCancelled -> IO (Either Error a)
    asynchandler _ = return $ Left Exit
    ehandler :: E.AsyncException -> IO (Either Error a)
    ehandler _ = return $ Left Exit
    iohandler :: E.IOException -> IO (Either Error a)
    iohandler err = return . Left . IOError . show $ err
    otherhandler :: E.SomeException -> IO (Either Error a)
    otherhandler err = return . Left . SomeError . show $ err

throwConfig :: (MError m) => String -> [String] -> m a
throwConfig str args = throw $ ConfigError $ template str args