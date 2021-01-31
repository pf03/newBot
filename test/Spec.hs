
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Logic
import Types 
import Control.Monad.State.Lazy

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
        map toMessageCommand testHelp `allShouldBe` Right Help
      it "returns repeat command" $ do
        map toMessageCommand testRepeat `allShouldBe` Right Repeat
      it "returns start command" $ do
        map toMessageCommand testStart `allShouldBe` Right Start
      it "returns button command" $ do
        map toMessageCommand testButton `eachShouldBe` map (Right . Button) resultButton
      it "returns unknown command" $ do
        map toMessageCommand testUnknown `eachShouldBe` map (Right . Unknown) resultUnknown
        --map toMessageCommand testUnknown `shouldBe` map (Right . Unknown ) resultUnknown
      it "returns message" $ do
        map toMessageCommand testMessages `eachShouldBe` map Left testMessages

--toMessageCommand
testTextAnswer :: Spec
testTextAnswer = do
    describe "Logic.textAnswer" $ do 
      it "returns something1" $ do
        textAnswerCase `evalStateShouldBe` (textAnswerResult `withInitialState` someState) 
      it "returns something2" $ do
        textAnswerCases `allEvalStatesShouldBe` (textAnswerResult `withInitialState` someState) 
      it "returns something3" $ do
        textAnswerCases `eachEvalStateShouldBe` (textAnswerResults `withInitialState` someState) 


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

--ALL of cases SHOULD BE eqaul to one result
allShouldBe :: (HasCallStack, Show a, Eq a) => [a] -> a -> Expectation
allShouldBe cases result = eachShouldBe cases (replicate (length cases) result)

--EACH of cases SHOULD BE eqaul to each of results
eachShouldBe :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Expectation
eachShouldBe cases results = bimapM_ shouldBe cases results


--Проверить, изменяется ли State в этих функциях!!! 
--bimapM_ нельзя использовать в этих функциях, т. к. он не учитывает эффект State
--ALL EVAL STATES of cases SHOULD BE eqaul to one result
allEvalStatesShouldBe :: (HasCallStack, Show a, Eq a) => [State s a] -> (a, s) -> Expectation
allEvalStatesShouldBe states (result, initialState) = eachEvalStateShouldBe states (replicate (length states) result, initialState)

--EACH EVAL STATE of cases SHOULD BE eqaul to each of results
eachEvalStateShouldBe :: (HasCallStack, Show a, Eq a) => [State s a] -> ([a], s) -> Expectation
eachEvalStateShouldBe [] ([], _) = return () 
eachEvalStateShouldBe (s:ss) (r:rs, initialState) = do
  let (a, modifiedState) = runState s initialState
  a `shouldBe` r
  eachEvalStateShouldBe ss (rs, modifiedState)
eachEvalStateShouldBe _ _ = error "lists of tests and answers must have equal lengths"

--EVAL STATE of case SHOULD BE equal to result WITH INITIAL STATE
evalStateShouldBe :: (HasCallStack, Show a, Eq a) => State s a -> (a, s) -> Expectation
evalStateShouldBe state (result, initialState) = eachEvalStateShouldBe [state] ([result], initialState)

textAnswerCases :: [State S Message]
textAnswerCases = undefined

textAnswerCase :: State S Message
textAnswerCase = undefined

textAnswerResults :: [Message]
textAnswerResults = undefined

textAnswerResult :: Message
textAnswerResult = undefined


someState :: S
someState = undefined



withInitialState = (,)

--this is not equal to base Data.Bifoldable.bimapM_
bimapM_ :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()
bimapM_ f [] [] = return ()
bimapM_ f (x:xs) (y:ys) = do
  f x y
  bimapM_ f xs ys
bimapM_ _ _ _ = error "list args of bimapM_ must have equal lengths"


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