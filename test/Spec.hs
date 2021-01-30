
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Logic
import Types 

main :: IO ()
main = hspec testToMessageCommand
  -- hspec $ do
  -- describe "Logic.testingFunction" $ do
  --   it "returns the first element of a list" $ do
  --     testingFunction [23 ..] `shouldBe` (23 :: Int)

  --   it "returns the first element of an *arbitrary* list" $
  --     property $ \x xs -> testingFunction (x:xs) == (x :: Int)

  --   it "throws an exception if used with an empty list" $ do
  --     evaluate (testingFunction []) `shouldThrow` anyException

--toMessageCommand
testToMessageCommand :: Spec
testToMessageCommand = do
    describe "Logic.toMessageCommand" $ do 
      it "returns help command" $ do
        map toMessageCommand testHelp `shouldBeAllEqual` Right Help
      it "returns repeat command" $ do
        map toMessageCommand testRepeat `shouldBeAllEqual` Right Repeat
      it "returns start command" $ do
        map toMessageCommand testStart `shouldBeAllEqual` Right Start
      it "returns button command" $ do
        map toMessageCommand testButton `shouldBeAll` map (Right . Button ) resultButton
      it "returns unknown command" $ do
        map toMessageCommand testUnknown `shouldBeAll` map (Right . Unknown ) resultUnknown
        --map toMessageCommand testUnknown `shouldBe` map (Right . Unknown ) resultUnknown
      it "returns message" $ do
        map toMessageCommand testMessages `shouldBeAll` map Left testMessages

testMessages = [
  "", " ", "foo", "кириллица", "  kj mkl kl ", " ?/sd", "sd /sd", "a/ssdf", "s8/*-*/*-4",
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

testHelp = ["/help", " /help", "/help ", "         /help   "]
testRepeat = ["/repeat", " /repeat", "/repeat ", "         /repeat   "]
testStart = ["/start", " /start", "/start ", "         /start   "]
--наши команды не имеют никаких параметров, поэтому команды с параметрами относим к неизвестнымю Возможно следует отказаться от lowerCase
testUnknown = ["/unk", " /unk", "/unk ", "         /unk   ", 
    "/other", "/withparams 1 2 3", "/a", "/6" ,"/0", "/3 1", "/repeat 3", "/help 3",  "/Repeat", "/REPEAT", "//repeat"
  ]
resultUnknown = ["unk", "unk", "unk", "unk",
    "other", "withparams", "a", "6", "0", "3", "repeat", "help", "Repeat", "REPEAT", "/repeat"]

testButton = ["/1", "/2", "/3", "/4", "/5",
    " /1", " /2", " /3", " /4", " /5",
    " /1 ", " /2 ", " /3 ", " /4 ", " /5 "
  ]
resultButton :: [Int]
resultButton = concat $ replicate 3 [1..5]

--может такое уже есть, хз
shouldBeAll :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Expectation
shouldBeAll [] [] = return ()
shouldBeAll (x:xs) (y:ys) = do
  x `shouldBe` y
  shouldBeAll xs ys
shouldBeAll _ _ = error "lists of tests and answers must have equal lengths"

shouldBeAllEqual :: (HasCallStack, Show a, Eq a) => [a] -> a -> Expectation
shouldBeAllEqual [] y = return ()
shouldBeAllEqual (x:xs) y = do
  x `shouldBe` y
  shouldBeAllEqual xs y



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