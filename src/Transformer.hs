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
import qualified State as S
import App

import System.Console.ANSI

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

--наш проект
import qualified Config --40
-- import Error
import Types  --100
-- import Parse
import Interface.Error
import qualified Interface.Log as Log
import  Interface.Cache as Cache
import Class


-----------------------------Instances-----------------------------------------
instance Log.MLog T where
  getSettings = getLogSettings
  setSettings = setLogSettings
  getConfig = getLogConfig
--   setConfig = S.setLogConfig
--   message = Log.messageIO

instance MError T where
    throw :: E -> T a
    throw e  = lift $ throwE e

    catch :: T a -> (E -> T a) -> T a
    catch ta f  = StateT $ \s -> catchE (runStateT ta s) $ \e -> runStateT (f e) s

instance MIOError T

instance MCache T where
    getCache = gets cache
    setCache c = modify (\st -> st {cache = c})

-- instance MT T


--Пользователи ничего не должны знать о внутренней структуре нашего трансформера.
--Для них есь три сущности - class ToTransformer, функции toT, runT ну и несколько вспомогательныйх функций типа printT



throwT :: E -> T a
throwT e  = toT (throwE e::Except E a)  



--запуск основного трансформера и всех монад попроще
runT :: (ToTransformer m, Show a) => m a -> IO()
runT m = do 
    let settings = (Cyan, True, "runT")
    es <- runExceptT Config.readS
    case es of 
        Left e -> do
            let dlc = Log.defaultConfig
            Log.text dlc settings Error "Ошибка считывания конфига: "
            Log.error dlc settings e
        Right s -> do 
            let cl = configLog s
            ea <- runExceptT $ runStateT (toT m) s
            case ea  of
                Left e -> do 
                    Log.text cl settings Error "Ошибка приложения: "
                    Log.error cl settings e
                Right a -> do 
                    Log.text cl settings Info "Результат: "
                    Log.ldata cl settings Data $ fst a


saveST :: T()
saveST = do
    --v <- toT readConfigValue 
    s <- get
    --let bc = encodeConfig v config
    toT $ Config.saveS s




testLog :: IO()
testLog = runT $ do
    Log.dataT Debug $ "Debug data value " ++ show [1..10]  :: T()
    Log.dataT Info $ "Info data value " ++ show [1..10] 
    Log.dataT Error $ "Error data value " ++ show [1..10] 
    Log.dataT Data $ "Data data value " ++ show [1..10] 
    Log.dataT Warning  $ "Warning data value " ++ show [1..10] 
    Log.colorTextT Blue Debug $"Blue color scheme " ++ klichko
    Log.colorTextT Cyan Debug $ "Cyan color scheme " ++ klichko
    Log.colorTextT Green Debug $ "Green color scheme " ++ klichko
    Log.colorTextT Yellow Debug $ "Yellow color scheme " ++ klichko
        where klichko = "Есть очень много по этому поводу точек зрения. Я четко придерживаюсь и четко понимаю, что те проявления, если вы уже так ребром ставите вопрос, что якобы мы"

---------------------------------------MonadLog-------------------------------------------------------
