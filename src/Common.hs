 module Common where
import Control.Monad.IO.Class
import Data.List.Split


----------------вспомогательные монадические функции -------------------------------------
ifJust :: Monad m => Maybe a -> m () -> m () 
ifJust ma m = case ma of 
      Just _ -> m
      _ -> return ()

ifNothing :: Monad m => Maybe a -> m () -> m () 
ifNothing ma m = case ma of 
      Nothing -> m
      _ -> return ()

printT :: (MonadIO m, Show a) => a -> m ()
printT = liftIO . print

putStrLnT :: MonadIO m => String -> m ()
putStrLnT = liftIO . putStrLn

----------------для работы со строками------------------------------------------------------
--подстановка в шаблон
template :: String -> [String] -> String
template str args = foldl f str $ zip ts args where
    ts = map (\n -> ('{':show n)++"}")[0,1..]
    f:: String -> (String, String) -> String
    f acc (t, arg) = replace t arg acc

--замена одной строки на другую
replace :: String -> String -> String -> String 
replace t s str = let strs = splitOn t str in
    concat $ init $ foldr (\part acc -> part:s:acc) [] strs 

-- test = template "Hello {0}, {1}, {0}, {1}," ["Petya", "Vasya"]
-- t1 = replace "{0}" "Vasya" "Hello {0}, {1}, {0}, {1}," 