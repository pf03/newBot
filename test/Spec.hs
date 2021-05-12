
-- Our modules
import           Common.Misc
import           Interface.MCache          as Cache
import           Logic.Logic              as Logic

-- Other modules
import           Control.Exception        (evaluate)
import           Control.Monad.State.Lazy
import qualified Data.Map.Internal        as M
import           Lib
import           Prelude                  hiding (repeat)
import           Test.Hspec

main :: IO ()
main = do
  hspec testToMessageCommand
  hspec testTextAnswer

-----------------------------Logic.toMessageCommand---------------------------------------------
testToMessageCommand :: Spec
testToMessageCommand = do
    describe "Logic.toMessageCommand" $ do
      it "returns message" $ do
        map Logic.toMessageCommand messages `eachShouldBe` map Left messages
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

helpCases :: [Either Message Command]
helpCases = map Logic.toMessageCommand
  ["/help", " /help", "/help ", "         /help   "]

startCases :: [Either Message Command]
startCases = map Logic.toMessageCommand
  ["/start", " /start", "/start ", "         /start   "]

repeatCases :: [Either Message Command]
repeatCases = map Logic.toMessageCommand
  ["/repeat", " /repeat", "/repeat ", "         /repeat   "]

buttonCases :: [Either Message Command]
buttonCases = map Logic.toMessageCommand
  ["/1", "/2", "/3", "/4", "/5",
    " /1", " /2", " /3", " /4", " /5",
    " /1 ", " /2 ", " /3 ", " /4 ", " /5 "]

buttonResults :: [Either Message Command]
buttonResults = map (Right . Button) $ concat $ replicate 3 [1..5]

unknownCases :: [Either Message Command]
unknownCases = map Logic.toMessageCommand
  ["/unk", " /unk", "/unk ", "         /unk   ",
    "/other", "/withparams 1 2 3", "/a", "/6", "/0", "/3 1", "/repeat 3", "/help 3",  "/Repeat", "/REPEAT", "//repeat",
    "/repeat 3", "/6", "/0", "/help 5 3",
    "  /repeat 3", " /6", " /0 ", "     /help  5    3  ",
    "/3 /repeat", "    /0 /repeat", "  /33 /repeat    ", "/6    /repeat"
  ]

unknownResults :: [Either Message Command]
unknownResults = map (Right . Unknown)
  ["unk", "unk", "unk", "unk",
    "other", "withparams 1 2 3", "a", "6", "0", "3 1", "repeat 3", "help 3", "Repeat", "REPEAT", "/repeat",
    "repeat 3", "6", "0", "help 5 3",
    "repeat 3", "6", "0", "help 5 3",
    "3 /repeat", "0 /repeat", "33 /repeat", "6 /repeat"
  ]

-------------------------------Logic.textAnswer-------------------------------------------------------------
testTextAnswer :: Spec
testTextAnswer = do
    describe "Logic.textAnswer" $ do
      it "have dialog with user" $ do
        textAnswerCases `eachEvalStateShouldBe` (textAnswerResults `withInitialState` someCache)

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

textAnswerCases :: [State Cache Message]
textAnswerCases = map (Logic.textAnswer someChatId . fst) textAnswerTuples

textAnswerResults :: [Message]
textAnswerResults = map snd textAnswerTuples

to :: a -> b -> (a, b)
to = (,)

textAnswerCase :: State Cache Message
textAnswerCase = Logic.textAnswer someChatId $ Left "hello bot"

textAnswerResult :: Message
textAnswerResult = "hello bot"

someConfigText :: ConfigText
someConfigText = ConfigText{
  help = "someHelpText",
  repeat = "someRepeatText",
  unknown = "someUnknownText",
  button = "someButtonText"
}

someConfigApp :: ConfigApp
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

someCache :: Cache
someCache = Cache {
    configApp = someConfigApp,
    configText = someConfigText,
    changed = False
}

someChatId :: ChatId
someChatId = 666