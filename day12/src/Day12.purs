module Day12 where

import Prelude

import Parser as P

import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array ((!!), (..))
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Either (Either)
import Data.Foldable (foldMap, sum)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Traversable (maximumBy)
import Data.Tuple (fst, snd)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)

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

newtype Rule = Rule { input :: Plants, output :: Plant }

ruleOutput :: Rule -> Plant
ruleOutput (Rule { output }) = output

type Rules = Array Rule

instance showRule :: Show Rule where
    show (Rule r) = show r.input <> " => " <> show r.output

ruleParser :: Parser Rule
ruleParser = do
    input <- P.zeroOrMore plantParser
    unless (Array.length input == 5) $ P.fail $ "Input not 5 long: " <> show input
    _ <- P.literal " => "
    output <- plantParser
    pure $ Rule { input: Plants input, output }

type Input = { initialState :: State, rules :: Rules }

inputParser :: Parser Input
inputParser = do
    initialState <- initialStateParser
    P.eol
    P.eol
    rules <- P.zeroOrMore (ruleParser <* P.eol)
    pure { initialState, rules }

parseInput :: String -> Either String Input
parseInput = P.runParser inputParser

doesRuleMatch :: Array Plant -> Rule -> Boolean
doesRuleMatch plants (Rule { input: Plants input }) = plants == input

matchingRuleOutput :: Rules -> State -> Int -> Plant
matchingRuleOutput rules (State { startIndex, plants: Plants plants }) centreIndex =
    let surroundings = (Array.index plants >>> fromMaybe No) <$> (centreIndex - startIndex - 2) .. (centreIndex - startIndex + 2) in
    let matchingRules = Array.filter (doesRuleMatch surroundings) rules in
    case matchingRules of
        [rule] -> ruleOutput rule
        [] -> No
        _ -> unsafeCrashWith $ "not exactly one matching rule at " <> show centreIndex <> " for " <> show surroundings

generation :: Rules -> State -> State
generation rules state@(State { startIndex, plants: Plants plants }) =
    let nextStartIndex = startIndex - 2 -- assume plants can't grow in isolation!
        nextEndIndex = startIndex + (Array.length plants) + 2 in
    let nextPlants = matchingRuleOutput rules state <$> nextStartIndex .. nextEndIndex in
    trimState $ State { plants: Plants nextPlants, startIndex: nextStartIndex }

generations :: Rules -> Int -> State -> Array State
generations rules n initialState = go [initialState]
    where
    go :: Array State -> Array State
    go states =
        let l = Array.length states in
        if l > n then states
        else
            go $ Array.snoc states (generation rules (unsafePartial $ Array.unsafeIndex states (l - 1)))

nthGeneration :: Rules -> Int -> State -> State
nthGeneration rules n initialState = go 0 initialState
    where
    go :: Int -> State -> State
    go i state =
        if i >= n then state
        else
            go (i + 1) (generation rules state)

sumPlantNumbers :: State -> Int
sumPlantNumbers (State { startIndex, plants: Plants plants }) =
    let plantsAndNumbers = Array.zip plants (startIndex .. (startIndex + Array.length plants)) in
    sum $ snd <$> Array.filter ((_ == Yes) <<< fst) plantsAndNumbers