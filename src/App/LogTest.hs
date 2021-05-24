module App.LogTest where

import qualified Interface.MLog.Exports as Log
import qualified System.Console.ANSI as Color
import qualified Transformer.Exports as Transformer

testLog :: IO ()
testLog = Transformer.run $ do
  Log.debugM $ "Debug data value " ++ show [1 .. 10 :: Int]
  Log.infoM $ "Info data value " ++ show [1 .. 10 :: Int]
  Log.warnM $ "warnM data value " ++ show [1 .. 10 :: Int]
  Log.errorM $ "Error data value " ++ show [1 .. 10 :: Int]
  Log.criticalM $ "criticalM data value " ++ show [1 .. 10 :: Int]
  Log.infoCM Color.Blue $ "Blue color scheme " ++ klichko
  Log.infoCM Color.Cyan $ "Cyan color scheme " ++ klichko
  Log.infoCM Color.Green $ "Green color scheme " ++ klichko
  Log.infoCM Color.Yellow $ "Yellow color scheme " ++ klichko
  where
    klichko = "Есть очень много по этому поводу точек зрения. Я четко придерживаюсь и четко понимаю, что те проявления, если вы уже так ребром ставите вопрос, что якобы мы"