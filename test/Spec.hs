import Common.Types (ChatId, Command (..), Message)
import Control.Monad.State.Lazy (State)
import qualified Data.Map.Internal as M
import qualified Interface.MCache.Exports as Cache
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

messages :: [Message]
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

-------------------------------Logic.textAnswer-------------------------------------------------------------
testTextAnswer :: Spec
testTextAnswer = do
  describe "Logic.textAnswer" $ do
    it "have dialog with user" $ do
      textAnswerCases `eachEvalStateShouldBe` (textAnswerResults `withInitialState` someCache)

--query - answer to bot
textAnswerTuples :: [(Either Message Command, [Message])]
textAnswerTuples =
  [ Left "hello bot" `to` replicate 2 "hello bot",
    Right Repeat `to` ["someRepeatText"],
    Right (Button 3) `to` ["someButtonText"],
    Left "How do you do?" `to` replicate 3 "How do you do?",
    Right Help `to` ["someHelpText"],
    Right Start `to` ["someHelpText"],
    Right Repeat `to` ["someRepeatText"],
    Right (Button 5) `to` ["someButtonText"],
    Left "5" `to` replicate 5 "5",
    Right (Unknown "someCommand") `to` ["someUnknownText"],
    Left "good bye" `to` replicate 5 "good bye"
  ]

textAnswerCases :: [State Cache.Cache [Message]]
textAnswerCases = map (Logic.textAnswer someChatId . fst) textAnswerTuples

textAnswerResults :: [[Message]]
textAnswerResults = map snd textAnswerTuples

to :: a -> b -> (a, b)
to = (,)

someConfigText :: Cache.ConfigText
someConfigText =
  Cache.ConfigText
    { Cache.help = "someHelpText",
      Cache.repeat = "someRepeatText",
      Cache.unknown = "someUnknownText",
      Cache.button = "someButtonText"
    }

someConfigApp :: Cache.ConfigApp
someConfigApp =
  Cache.ConfigApp
    { Cache.enable = True,
      Cache.name = "someName",
      Cache.app = Cache.VK,
      Cache.host = "someAppHost",
      Cache.token = "someAppToken",
      Cache.updateId = Just 666666,
      Cache.repeatNumber = M.empty,
      Cache.groupId = 999999,
      Cache.version = "someAppVersion"
    }

someCache :: Cache.Cache
someCache =
  Cache.Cache
    { Cache.configApp = someConfigApp,
      Cache.configText = someConfigText,
      Cache.defaultRepeatNumber = 2,
      Cache.changed = False
    }

someChatId :: ChatId
someChatId = 666