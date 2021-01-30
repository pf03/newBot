--importPriority = 70
module Error where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Trans.Except
import Types  --100


instance Show E where
    show (ParseError s) = "Ошибка парсинга JSON: "++s
    show (QueryError s) = "Ошибка веб-запроса: "++s
    show (ConfigError s) = "Ошибка чтения или парсинга файла конфигурации config.json: "++s
instance Exception E

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
    