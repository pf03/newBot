
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Logic
import Types 
import Control.Monad.State.Lazy
import Lib
import Common
import qualified Data.Map.Internal as M
import Prelude hiding (repeat)

main :: IO ()
main = do 
  hspec testToMessageCommand
  hspec testTextAnswer


-----------------------------toMessageCommand---------------------------------------------
testToMessageCommand :: Spec
testToMessageCommand = do
    describe "Logic.toMessageCommand" $ do 
      it "returns message" $ do
        map toMessageCommand messages `eachShouldBe` map Left messages
      it "returns help command" $ do
        helpCases `allShouldBe` Right Help
      it "returns start command" $ do
        startCases `allShouldBe` Right Start
      it "returns repeat command" $ do
        repeatCases `allShouldBe` Right Repeat
      it "returns button command" $ do
        buttonCases `eachShouldBe` buttonResults
      it "returns unknown command" $ do
        unknownCases `eachShouldBe` unknownResults
        --map toMessageCommand testUnknown `shouldBe` map (Right . Unknown ) resultUnknown


messages = 
  ["", " ", "foo", "кириллица", "  kj mkl kl ", " ?/sd", "sd /sd", "a/ssdf", "s8/*-*/*-4",
    "repeat",  
    "/ repeat", 
    " / repeat", 
    "/ repeat ", 
    " / repeat ", 
    "   / repeat       ", 
    " /    repeat ", 
    "/ re p e at ",
    "/ vasya"]

helpCases = map toMessageCommand
  ["/help", " /help", "/help ", "         /help   "]

startCases = map toMessageCommand
  ["/start", " /start", "/start ", "         /start   "]

repeatCases = map toMessageCommand
  ["/repeat", " /repeat", "/repeat ", "         /repeat   "]

buttonCases = map toMessageCommand 
  ["/1", "/2", "/3", "/4", "/5",
    " /1", " /2", " /3", " /4", " /5",
    " /1 ", " /2 ", " /3 ", " /4 ", " /5 "]

buttonResults = map (Right . Button) $ concat $ replicate 3 [1..5]

--наши команды не имеют никаких параметров, поэтому команды с параметрами относим к неизвестнымю Возможно следует отказаться от lowerCase
unknownCases = map toMessageCommand
  ["/unk", " /unk", "/unk ", "         /unk   ", 
    "/other", "/withparams 1 2 3", "/a", "/6" ,"/0", "/3 1", "/repeat 3", "/help 3",  "/Repeat", "/REPEAT", "//repeat"]

unknownResults = map (Right . Unknown)
  ["unk", "unk", "unk", "unk",
    "other", "withparams", "a", "6", "0", "3", "repeat", "help", "Repeat", "REPEAT", "/repeat"]


-------------------------------textAnswer-------------------------------------------------------------
testTextAnswer :: Spec
testTextAnswer = do
    describe "Logic.textAnswer" $ do 
      it "returns something1" $ do
        textAnswerCase `evalStateShouldBe` (textAnswerResult `withInitialState` someState) 
      -- it "returns something2" $ do
      --   textAnswerCases `allEvalStatesShouldBe` (textAnswerResult `withInitialState` someState) 
      it "have dialog with user" $ do
        textAnswerCases `eachEvalStateShouldBe` (textAnswerResults `withInitialState` someState) 


-- textAnswerCases :: [State S Message]
-- textAnswerCases = map (textAnswer someChatId) [
--     Left "hello bot" `to` "hello bot",
--     Right Repeat,
--     Right $ Button 3,
--     Left "How do you do?",
--     Right Help, 
--     Right Start,
--     Right Repeat,
--     Right $ Button 5,
--     Left  "5",
--     Right $ Unknown "someCommand",
--     Left "good bye"
--   ]


--query - answer to bot
textAnswerTuples :: [(Either Message Command, Message)]
textAnswerTuples =  [
    Left "hello bot" `to` "hello bot",
    Right Repeat `to` "someRepeatText",
    Right (Button 3) `to` "someButtonText",
    Left "How do you do?" `to` "How do you do? How do you do? How do you do?",
    Right Help `to` "someHelpText", 
    Right Start `to` "someHelpText",
    Right Repeat `to` "someRepeatText",
    Right (Button 5) `to` "someButtonText",
    Left  "5" `to` "5 5 5 5 5",
    Right (Unknown "someCommand") `to` "someUnknownText",
    Left "good bye" `to` "good bye good bye good bye good bye good bye"
  ]

textAnswerCases :: [State S Message]
textAnswerCases = map (textAnswer someChatId . fst) textAnswerTuples

textAnswerResults :: [Message]
textAnswerResults = map snd textAnswerTuples
to = (,)

textAnswerCase :: State S Message
textAnswerCase = textAnswer someChatId $ Left "hello bot" 


--такие варианты можно сразу в тест
textAnswerErrorCase1 :: State S Message
textAnswerErrorCase1 = textAnswer someChatId $ Right $ Button 6 
textAnswerErrorCase :: State S Message
textAnswerErrorCase = textAnswer someChatId $ Right $ Button 0 



textAnswerResult :: Message
textAnswerResult = "hello bot"

someState :: S
someState = S {
  app = notUsed "app",
  configApp = someConfigApp,
  configText = someConfigText,
  configLog = notUsed "configLog",
  logSettings = notUsed "logSettings"
}

someConfigText = ConfigText{
  help = "someHelpText", 
  repeat = "someRepeatText", 
  unknown = "someUnknownText", 
  button = "someButtonText"
}

someConfigApp = ConfigApp {
  name = "someAppName",
  host = "someAppHost",
  token = "someAppToken",
  updateId = 666666,
  updateIdFromFile = False,
  repeatNumber = M.empty,
  groupId = 999999,
  version = "someAppVersion"
}

someChatId = 666

--не знаю, как сделать по другому, чтобы не определять ненужные сущности
notUsed :: String -> a
notUsed field = error $ template "Field {0} should not be used in S" [field]

  -- hspec $ do
  -- describe "Logic.testingFunction" $ do
  --   it "returns the first element of a list" $ do
  --     testingFunction [23 ..] `shouldBe` (23 :: Int)

  --   it "returns the first element of an *arbitrary* list" $
  --     property $ \x xs -> testingFunction (x:xs) == (x :: Int)

  --   it "throws an exception if used with an empty list" $ do
  --     evaluate (testingFunction []) `shouldThrow` anyException

-- simpleProperties = do
--   it "lastDigit [x] == x `mod` 10" $
--     property (\(NonNegative x) -> _ [x] == x `mod` 10)
--   it "lastDigit [x, y] == x ^ y `mod` 10" $
--     property (\(NonNegative x) (NonNegative y) -> _ [x, y] == x ^ y `mod` 10)


-- Правильно ли бот обработает команды:
-- /repeat
-- / repeat
--  /repeat
--  / repeat
-- /repea
-- /repe at
-- /qwe
-- Правильно ли отреагирует (сообщением об ошибке), если установить число повторов больше 5 или меньше 1.