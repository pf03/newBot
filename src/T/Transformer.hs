--{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
--importPriority = 20
module T.Transformer where 
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
import qualified Data.Aeson.Encode.Pretty   as Aeson
import qualified System.Console.ANSI              as Color
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

--Пользователи ничего не должны знать о внутренней структуре нашего трансформера.
--Для них есь три сущности - class ToTransformer, функции toT, runT ну и несколько вспомогательныйх функций типа printT



-- throwT :: E -> T a
-- throwT e  = toT (throwE e::Except E a)  


-- скопировать более строгую версию из сервера
--запуск основного трансформера и всех монад попроще
runT :: Show a => T a -> IO()
runT m = do 
    let settings = LogSettings Cyan True "runT"
    es <- runExceptT (readS :: ExceptT E IO S)
    case es of 
        Left e -> do
            let dlc = Log.defaultConfig
            Log.critical dlc settings "Ошибка считывания конфига: "
            Log.critical dlc settings $ show e
        Right s -> do 
            let cl = configLog s
            ea <- runExceptT $ runStateT m s
            case ea  of
                Left e -> do 
                    Log.error cl settings"Application error: "
                    Log.error cl settings $ show e
                Right a -> do 
                    Log.info cl settings "Result: "
                    Log.info cl settings $ show . fst $ a


saveST :: T()
saveST = do
    --v <- toT readConfigValue 
    s <- get
    --let bc = encodeConfig v config
    saveS s


-----------------------------Log test------------------------------------------
testLog :: IO()
testLog = runT $ do
    Log.debugM $ "Debug data value " ++ show [1..10::Int]  :: T()
    Log.infoM $ "Info data value " ++ show [1..10::Int]
    Log.warnM  $ "warnM data value " ++ show [1..10::Int]
    Log.errorM $ "Error data value " ++ show [1..10::Int]
    Log.criticalM  $ "criticalM data value " ++ show [1..10::Int]
    Log.infoCM Color.Blue $ "Blue color scheme " ++ klichko
    Log.infoCM Color.Cyan $ "Cyan color scheme " ++ klichko
    Log.infoCM Color.Green $ "Green color scheme " ++ klichko
    Log.infoCM Color.Yellow $ "Yellow color scheme " ++ klichko
        where klichko = "Есть очень много по этому поводу точек зрения. Я четко придерживаюсь и четко понимаю, что те проявления, если вы уже так ребром ставите вопрос, что якобы мы"





