--{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
--importPriority = 20
module Transformer where 
--mtl

-- Our modules
import qualified Logic.Config --40
-- import Error
-- import Parse
import Interface.MError as Error
import Interface.MLog as Log
import Interface.MCache as Cache
import Interface.MT
import Logic.Config

-- Other modules
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Data.Maybe
import T.State as S

import System.Console.ANSI

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC






--Пользователи ничего не должны знать о внутренней структуре нашего трансформера.
--Для них есь три сущности - class ToTransformer, функции toT, runT ну и несколько вспомогательныйх функций типа printT



throwT :: E -> T a
throwT e  = toT (throwE e::Except E a)  


-- скопировать более строгую версию из сервера
--запуск основного трансформера и всех монад попроще
runT :: Show a => m a -> IO()
runT m = do 
    let settings = (Cyan, True, "runT")
    es <- runExceptT readS
    case es of 
        Left e -> do
            let dlc = Log.defaultConfig
            Log.text dlc settings Error "Ошибка считывания конфига: "
            Log.error dlc settings e
        Right s -> do 
            let cl = configLog s
            ea <- runExceptT $ runStateT m s
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
    toT $ saveS s




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

-------------------State <-> Config--------------------------------------
-- это в другой модуль
-- readS :: ExceptT E IO S
-- readS = do
--     config <- readConfig
--     let s = toS config
--     --print config
--     return s


-- -- !!!!!!!!!!!!тут нужно переделать, чтобы не считывать каждый раз конфиг заново, а запоминать его!!!!!!!!!!!!!!
saveS :: MIOError m => S -> m ()
saveS s = do
    config <- readConfig
    let newConfig = fromS config s
    ExceptT $ Error.toEE (L.writeFile pathConfig (encode newConfig)) `catch` hW


toS :: Config -> S
toS configFile = let 
        configApps =  _apps configFile;
        configLog = _log configFile
        configApp = head $ filter (\ca -> show (_app configFile) == name ca) configApps in 
    S {
        app = _app configFile,
        configApp = configApp,
        configText = _text configFile,
        configLog = configLog,
        logSettings = Log.defaultSettings
    }

fromS :: Config -> S -> Config
fromS configFile config = let 
        configApps =  _apps configFile;
        newConfigApps = [configApp config] <> filter (\ca -> name ca /= name (configApp config) ) configApps in
    configFile {_apps = newConfigApps}

