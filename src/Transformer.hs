--{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
--importPriority = 20
module Transformer where 
--mtl
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Data.Maybe
import App

import System.Console.ANSI

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

--наш проект
import Config --40
-- import Error
import Types  --100
-- import Parse
import Logic  --30
import Error
import Log
import Class



--Пользователи ничего не должны знать о внутренней структуре нашего трансформера.
--Для них есь три сущности - class ToTransformer, функции toT, runT ну и несколько вспомогательныйх функций типа printT



throwT :: E -> T a
throwT e  = toT (throwE e::Except E a)  



--запуск основного трансформера и всех монад попроще
runT :: (ToTransformer m, Show a) => m a -> IO()
runT m = do 
    let settings = (Cyan, True, "runT")
    es <- runExceptT readS
    case es of 
        Left e -> do
            let dlc = defaultLogConfig
            logText dlc settings Error "Ошибка считывания конфига: "
            logError dlc settings e
        Right s -> do 
            let cl = configLog s
            ea <- runExceptT $ runStateT (toT m) s
            case ea  of
                Left e -> do 
                    logText cl settings Error "Ошибка приложения: "
                    logError cl settings e
                Right a -> do 
                    logText cl settings Info "Результат: "
                    logData cl settings Data $ fst a


saveST :: T()
saveST = do
    --v <- toT readConfigValue 
    s <- get
    --let bc = encodeConfig v config
    toT $ saveS s

instance MonadLog T where 
  getLogSettings = gets logSettings
  setLogSettings ls = modify $ \s -> s {logSettings = ls}
  --resetLogSettings = modify $ \s -> s {logSettings = defaultLogSettings } --Log.hs
  getLogConfig = gets configLog

testLog :: IO()
testLog = runT $ do
    logDataT Debug $ "Debug data value " ++ show [1..10]  :: T()
    logDataT Info $ "Info data value " ++ show [1..10] 
    logDataT Error $ "Error data value " ++ show [1..10] 
    logDataT Data $ "Data data value " ++ show [1..10] 
    logDataT Warning  $ "Warning data value " ++ show [1..10] 
    logColorTextT Blue Debug $"Blue color scheme " ++ klichko
    logColorTextT Cyan Debug $ "Cyan color scheme " ++ klichko
    logColorTextT Green Debug $ "Green color scheme " ++ klichko
    logColorTextT Yellow Debug $ "Yellow color scheme " ++ klichko
        where klichko = "Есть очень много по этому поводу точек зрения. Я четко придерживаюсь и четко понимаю, что те проявления, если вы уже так ребром ставите вопрос, что якобы мы"