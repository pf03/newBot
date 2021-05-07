{-# LANGUAGE FlexibleInstances #-}
module Interface.Error where


-- Our modules
import Common.Misc

-- Other modules
import Control.Monad.Except
import Control.Monad.Trans.Except ( catchE, except, throwE )
import           Data.Aeson



import qualified Control.Exception          as E

-----------------------------Types---------------------------------------------
data E = ParseError String
    | QueryError String
    -- | RequestError String
    | ConfigError String
    -- | DBError String
    | IOError String
    -- | AuthError String
    -- | An error that should never occur when the program is written correctly
    -- (for example, incorrect pattern matching)
    | DevError String 
    | SomeError String

-- это надо убрать, раз мы пользуемся мондами Parser - Except - ExceptT
type ES = Either String
type EE = Either E

instance Show E where
    show (ParseError s) = "Ошибка парсинга JSON: "++s
    show (QueryError s) = "Ошибка веб-запроса: "++s
    show (ConfigError s) = "Ошибка чтения или парсинга файла конфигурации config.json: "++s
instance E.Exception E

--конкретизируем тип ошибки конструктором c
typeError :: (String -> E) -> ES a  -> EE a
typeError c (Left s) = Left $ c s
typeError _ (Right a) = Right a

toEE :: Monad m => m a -> m (EE a)
toEE x = return <$> x

toE :: Monad m => Except E a -> ExceptT E m a 
toE = except . runExcept

findPosition :: [Char] -> Integer
findPosition str = error "todo"

-- --пример  check 7 4 "2569 1112570 1112571 1112572 " = True, т.е. строка начинается с m-подчисла n-числа
-- --пример  check 4 2 "99 1000 1001 1002 10" = True, граничный случай
-- check :: Int -> Int -> String -> Bool
-- check m n str = let 
--     mstr = take m str;
--     nstr' = drop m str;
--     n'= read nstr';
--     n = if 
--     in

-- from  Server!!!!!!!!!!!!!
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

-----------------------------Decode--------------------------------------------
-- eDecode :: (MError m, FromJSON a) => LBS -> m a
-- eDecode bs = catchEither (eitherDecode bs) ParseError