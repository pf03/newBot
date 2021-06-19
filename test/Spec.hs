import Common.Types (ChatId, Command (..), Label (..), Message (..))
import Control.Monad.State.Lazy (State)
import qualified Data.Map.Internal as M
import qualified Interface.Cache.Config.Exports as Config
import qualified Interface.Cache.Exports as Cache
import Lib
  ( allShouldBe,
    eachEvalStateShouldBe,
    eachShouldBe,
    withInitialState,
  )
import qualified Logic.Logic as Logic
import Test.Hspec (Spec, describe, hspec, it)
import Prelude hiding (repeat)

main :: IO ()
main = do
  hspec testToMessageCommand
  hspec testEvalCommandAnswer

-----------------------------Logic.toMessageCommand---------------------------------------------
testToMessageCommand :: Spec
testToMessageCommand = do
  describe "Logic.toMessageCommand" $ do
    it "returns message" $ do
      map Logic.toMessageCommand messages `eachShouldBe` map (Left . Message) messages
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

messages :: [String]
messages =
  [ "",
    " ",
    "foo",
    "кириллица",
    "  kj mkl kl ",
    " ?/sd",
    "sd /sd",
    "a/ssdf",
    "s8/*-*/*-4",
    "repeat",
    "/ repeat",
    " / repeat",
    "/ repeat ",
    " / repeat ",
    "   / repeat       ",
    " /    repeat ",
    "/ re p e at ",
    "/ vasya"
  ]

helpCases :: [Either Message Command]
helpCases =
  map
    Logic.toMessageCommand
    ["/help", " /help", "/help ", "         /help   "]

startCases :: [Either Message Command]
startCases =
  map
    Logic.toMessageCommand
    ["/start", " /start", "/start ", "         /start   "]

repeatCases :: [Either Message Command]
repeatCases =
  map
    Logic.toMessageCommand
    ["/repeat", " /repeat", "/repeat ", "         /repeat   "]

buttonCases :: [Either Message Command]
buttonCases =
  map
    Logic.toMessageCommand
    [ "/1",
      "/2",
      "/3",
      "/4",
      "/5",
      " /1",
      " /2",
      " /3",
      " /4",
      " /5",
      " /1 ",
      " /2 ",
      " /3 ",
      " /4 ",
      " /5 "
    ]

buttonResults :: [Either Message Command]
buttonResults = map (Right . Button) $ concat $ replicate 3 [1 .. 5]

unknownCases :: [Either Message Command]
unknownCases =
  map
    Logic.toMessageCommand
    [ "/unk",
      " /unk",
      "/unk ",
      "         /unk   ",
      "/other",
      "/withparams 1 2 3",
      "/a",
      "/6",
      "/0",
      "/3 1",
      "/repeat 3",
      "/help 3",
      "/Repeat",
      "/REPEAT",
      "//repeat",
      "/repeat 3",
      "/6",
      "/0",
      "/help 5 3",
      "  /repeat 3",
      " /6",
      " /0 ",
      "     /help  5    3  ",
      "/3 /repeat",
      "    /0 /repeat",
      "  /33 /repeat    ",
      "/6    /repeat"
    ]

unknownResults :: [Either Message Command]
unknownResults =
  map
    (Right . Unknown)
    [ "unk",
      "unk",
      "unk",
      "unk",
      "other",
      "withparams 1 2 3",
      "a",
      "6",
      "0",
      "3 1",
      "repeat 3",
      "help 3",
      "Repeat",
      "REPEAT",
      "/repeat",
      "repeat 3",
      "6",
      "0",
      "help 5 3",
      "repeat 3",
      "6",
      "0",
      "help 5 3",
      "3 /repeat",
      "0 /repeat",
      "33 /repeat",
      "6 /repeat"
    ]

-------------------------------Logic.evalCommandAnswer-------------------------------------------------------------
testEvalCommandAnswer :: Spec
testEvalCommandAnswer = do
  describe "Logic.evalCommandAnswer" $ do
    it "answer to commands" $ do
      evalCommandAnswerCases `eachEvalStateShouldBe` (evalCommandAnswerResults `withInitialState` someCache)

--query - answer to bot
evalCommandAnswerTuples :: [(Command, (Message, [Label]))]
evalCommandAnswerTuples =
  [ Repeat `to` ("someRepeatText", map Label ["/1", "/2", "/3", "/4", "/5"]),
    Button 3 `to` ("someButtonText", []),
    Help `to` ("someHelpText", []),
    Start `to` ("someHelpText", []),
    Repeat `to` ("someRepeatText", map Label ["/1", "/2", "/3", "/4", "/5"]),
    Button 5 `to` ("someButtonText", []),
    Unknown "someCommand" `to` ("someUnknownText", [])
  ]

evalCommandAnswerCases :: [State Cache.Cache (Message, [Label])]
evalCommandAnswerCases = map (Logic.evalCommandAnswer someChatId . fst) evalCommandAnswerTuples

evalCommandAnswerResults :: [(Message, [Label])]
evalCommandAnswerResults = map snd evalCommandAnswerTuples

to :: a -> b -> (a, b)
to = (,)

someConfigText :: Config.ConfigText
someConfigText =
  Config.ConfigText
    { Config.textHelp = "someHelpText",
      Config.textRepeat = "someRepeatText",
      Config.textUnknown = "someUnknownText",
      Config.textButton = "someButtonText"
    }

someConfigApp :: Config.ConfigApp
someConfigApp =
  Config.ConfigApp
    { Config.appEnable = True,
      Config.appName = "someName",
      Config.appApp = Config.VK,
      Config.appHost = "someAppHost",
      Config.appToken = "someAppToken",
      Config.appUpdateId = Just 666666,
      Config.appRepeatNumber = M.empty,
      Config.appGroupId = 999999,
      Config.appVersion = "someAppVersion"
    }

someCache :: Cache.Cache
someCache =
  Cache.Cache
    { Cache.cacheConfigApp = someConfigApp,
      Cache.cacheConfigText = someConfigText,
      Cache.cacheDefaultRepeatNumber = 2,
      Cache.cacheChanged = False
    }

someChatId :: ChatId
someChatId = 666