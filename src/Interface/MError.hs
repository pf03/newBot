{-# LANGUAGE FlexibleInstances #-}
module Interface.MError where

import           Control.Monad.Except
import           Control.Monad.Trans.Except (catchE, throwE)
import qualified Control.Exception          as E

-----------------------------Types---------------------------------------------
data E = ParseError String
    | QueryError String
    | ConfigError String
    | IOError String
    -- | An error that should never occur when the program is written correctly
    -- (for example, incorrect pattern matching)
    | DevError String
    | SomeError String

type EE = Either E

instance Show E where
    show (ParseError s)  = "Parse JSON error: "++s
    show (QueryError s)  = "Query error: "++s
    show (ConfigError s) = "Config error: "++s
    show (IOError s)     = "IO error: "++s
    show (DevError s)    = "Developer error: "++s
    show (SomeError s)   = "Some error: "++s
instance E.Exception E

-----------------------------MError--------------------------------------------
class MonadFail m => MError m where
    throw :: E -> m a
    catch :: m a -> (E -> m a) -> m a

liftE ::  MError m => Either E a -> m a
liftE ea = case ea of
    Left e  -> throw e
    Right a -> return a

catchEither :: MError m => Either b a -> (b -> E) -> m a
catchEither eba handler = case eba of
        Left b  -> throw $ handler b
        Right a -> return a

toEither :: MError m => m a -> m (Either E a)
toEither ma = do
    catch (Right <$> ma) $ \e -> return $ Left e

-----------------------------MIOError------------------------------------------
class (MError m, MonadIO m) => MIOError m

catchEIO :: (MIOError m, E.Exception e) => IO a -> (e -> E) -> m a
catchEIO m h = do
    ea <- liftIO $  (Right <$> m) `E.catch` handler
    liftE ea
    where
    --handler :: Exception e => (e -> IO (EE a))
    handler e = return . Left  . h $ e

-- * The same as previous, but errors are handled automatically, without user handlers
liftEIO :: MIOError m => IO a -> m a
liftEIO m = do
    ea <- liftIO $  (Right <$> m) `E.catch` iohandler `E.catch` otherhandler
    liftE ea
    where
    iohandler :: E.IOException -> IO (EE a)
    iohandler e = return . Left  . IOError . show $ e
    otherhandler :: E.SomeException -> IO (EE a)
    otherhandler e = return . Left . SomeError . show $ e

-----------------------------Either E------------------------------------------
instance MonadFail (Either E) where
    fail s = Left $ DevError s

instance MError (Either E) where
    throw = Left
    catch ma f = case ma of
            Left e  -> f e
            Right a -> Right a

-----------------------------ExceptT E IO--------------------------------------
instance MError (ExceptT E IO) where
    throw = throwE
    catch = catchE

instance MIOError (ExceptT E IO)
