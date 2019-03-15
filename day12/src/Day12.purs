module Day12 where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array ((!!), (..))
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Either (Either)
import Data.Foldable (all, foldMap, sum)
import Data.Int.Bits (shl)
import Data.List.Lazy as LL
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (unfoldr)
import Parser as P
import Partial.Unsafe (unsafePartial)

data Plant = No | Yes

derive instance eqPlant :: Eq Plant

instance showPlant :: Show Plant where
    show No = "."
    show Yes = "#"

newtype Plants = Plants (Array Plant)

instance showPlants :: Show Plants where
    show (Plants plants) = foldMap show plants

newtype State = State { startIndex :: Int, plants :: Plants }

instance showState :: Show State where
    show (State s) = show s

trimStateStart :: State -> State
trimStateStart (State { startIndex, plants: Plants plants }) =
    let { init, rest } = Array.span (_ == No) plants in
    State { startIndex: startIndex + Array.length init, plants: Plants rest }

trimStateEnd :: State -> State
trimStateEnd state@(State s@{ plants: Plants plants }) =
    case Array.elemLastIndex Yes plants of
        Just i -> State s { plants = Plants (Array.take (i + 1) plants) }
        Nothing -> state

trimState :: State -> State
trimState = trimStateStart >>> trimStateEnd

type Parser = P.Parser Char

plantParser :: Parser Plant
plantParser = do
    c <- P.char
    case c of
        '.' -> pure No
        '#' -> pure Yes
        _ -> P.fail $ "Invalid plant char '" <> show c <> "'"

initialStateParser :: Parser State
initialStateParser = do
    _ <- P.literal "initial state: "
    plants <- P.zeroOrMore plantParser
    pure $ State { startIndex: 0, plants: Plants plants }

newtype Rule = Rule { input :: Array Plant, output :: Plant }

instance showRule :: Show Rule where
    show (Rule r) = show r.input <> " => " <> show r.output

ruleOutput :: Rule -> Plant
ruleOutput (Rule { output }) = output

plantValue :: Plant -> Int
plantValue No = 0
plantValue Yes = 1

sliceValue :: Array Plant -> Int -> Int
sliceValue plants startIndex =
    bit 4 + bit 3 + bit 2 + bit 1 + bit 0
    where
    bit n = plants !! (startIndex + 4 - n) # fromMaybe No # plantValue # (_ `shl` n)
--foldl (\v p -> (v `shl` 1) + (plantValue p)) 0 plants

type Rules = Array Plant

makeRules :: Array Rule -> Rules
makeRules ruleArray = ST.run (STArray.withArray go (Array.replicate 32 No))
    where
    go :: forall r. STArray r Plant -> ST r Unit
    go rules = ST.foreach ruleArray \(Rule { input, output }) ->
        void $ STArray.poke (sliceValue input 0) output rules

ruleParser :: Parser Rule
ruleParser = do
    input <- P.zeroOrMore plantParser
    unless (Array.length input == 5) $ P.fail $ "Input not 5 long: " <> show input
    _ <- P.literal " => "
    output <- plantParser
    pure $ Rule { input, output }

type Input = { initialState :: State, rules :: Rules }

inputParser :: Parser Input
inputParser = do
    initialState <- initialStateParser
    P.eol
    P.eol
    rules <- P.zeroOrMore (ruleParser <* P.eol)
    pure { initialState, rules: makeRules rules }

parseInput :: String -> Either String Input
parseInput = P.runParser inputParser

rulesOutput :: Rules -> Array Plant -> Int -> Plant
rulesOutput rules plants startIndex =
    let plantsValue = sliceValue plants startIndex in
    fromMaybe No $ rules !! plantsValue

matchingRuleOutput :: Rules -> State -> Int -> Plant
matchingRuleOutput rules (State { startIndex, plants: Plants plants }) centreIndex =
    rulesOutput rules plants (centreIndex - startIndex - 2)

generation :: Rules -> State -> State
generation rules state@(State { startIndex, plants: Plants plants }) =
    let nextStartIndex = startIndex - 2 -- assume plants can't grow in isolation!
        nextEndIndex = startIndex + (Array.length plants) + 2 in
    let nextPlants = matchingRuleOutput rules state <$> nextStartIndex .. nextEndIndex in
    trimState $ State { plants: Plants nextPlants, startIndex: nextStartIndex }

generations :: Rules -> State -> LL.List State
generations rules initialState = LL.iterate (generation rules) initialState

nthGeneration :: Rules -> Int -> State -> State
nthGeneration rules n initialState = unsafePartial $ fromJust $ LL.index (generations rules initialState) n

sumPlantNumbers :: State -> Int
sumPlantNumbers (State { startIndex, plants: Plants plants }) =
    let plantsAndNumbers = Array.zip plants (startIndex .. (startIndex + Array.length plants)) in
    sum $ snd <$> Array.filter ((_ == Yes) <<< fst) plantsAndNumbers

slidingWindow :: forall a. Int -> LL.List a -> LL.List (LL.List a)
slidingWindow n = unfoldr (\xs -> Tuple (LL.take n xs) <$> (LL.tail xs))

iterateGenerationsUntilStableDiff :: Rules -> State -> { generation :: Int, sum :: Int, diff :: Int }
iterateGenerationsUntilStableDiff rules initialState =
    let iterations = generations rules initialState <#> sumPlantNumbers # LL.zipWith makeIteration (LL.iterate (_+1) 0) in
    let pairs = LL.zipWith makePair (unsafePartial $ fromJust $ LL.tail iterations) iterations in
    let diffs = pairs <#> diff in
    let windows = slidingWindow 5 diffs in
    unsafePartial $ fromJust $ fromJust $ LL.head <$> (LL.head $ LL.dropWhile (not isStable) windows)

    where
        makeIteration generation sum = { generation, sum }
        makePair current prev = { current, prev }
        diff { current, prev } = { sum: current.sum, diff: (current.sum - prev.sum), generation: current.generation }
        isStable diffs = LL.uncons diffs <#> (\{head,tail} -> all (\d -> d.diff == head.diff) tail) # fromMaybe true