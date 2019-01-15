module Day4 where

import Prelude

import Control.Alt ((<|>))
import Data.Array (foldM, foldl, sortWith)
import Data.Array as Array
import Data.Either (Either(..))
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Hashable (class Hashable)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.Yarn (lines)
import Data.Traversable (class Foldable, maximumBy, sequence, sum)
import Data.Tuple (Tuple(..), fst, snd)
import Parser (Parser, eof, integer, literal, runParser)

newtype Date = Date {
    year :: Int,
    month :: Int,
    day :: Int,
    hour :: Int,
    minute :: Int
}

instance dateEq :: Eq Date where
    eq (Date d1) (Date d2) = eq d1 d2

instance dateOrd :: Ord Date where
    compare (Date d1) (Date d2) = compare [d1.year, d1.month, d1.day, d1.hour, d1.minute] [d2.year, d2.month, d2.day, d2.hour, d2.minute]

instance dateShow :: Show Date where
    show (Date d) = show d

data Entry
    = BeginsShift Int
    | FallsAsleep
    | WakesUp

instance entryShow :: Show Entry where
    show (BeginsShift id) = "BeginsShift " <> (show id)
    show FallsAsleep = "FallsAsleep"
    show WakesUp = "WakesUp"

type Line = {
    date :: Date,
    entry :: Entry
}

parseDate :: Parser Date
parseDate = do
    year <- integer
    _ <- literal "-"
    month <- integer
    _ <- literal "-"
    day <- integer
    _ <- literal " "
    hour <- integer
    _ <- literal ":"
    minute <- integer
    pure $ Date {year, month, day, hour, minute}

parseBeginsShift :: Parser Entry
parseBeginsShift = do
    _ <- literal "Guard #"
    id <- integer
    _ <- literal " begins shift"
    pure $ BeginsShift id

parseFallsAsleep :: Parser Entry
parseFallsAsleep = literal "falls asleep" $> FallsAsleep

parseWakesUp :: Parser Entry
parseWakesUp = literal "wakes up" $> WakesUp

parseEntry :: Parser Entry
parseEntry = parseBeginsShift <|> parseFallsAsleep <|> parseWakesUp

parseLine :: Parser Line
parseLine = do
    _ <- literal "["
    date <- parseDate
    _ <- literal "] "
    entry <- parseEntry
    eof
    pure {date, entry}

parseInput :: String -> Either String (Array Line)
parseInput input = lines input <#> runParser parseLine # sequence

sortLines :: Array Line -> Array Line
sortLines = sortWith (_.date)

type GuardId = Int
type Minute = Int

type Shift = {
    year :: Int,
    month :: Int,
    day :: Int,
    id :: GuardId,
    sleeps :: Array {
        startMinute :: Minute,
        endMinute :: Minute -- exclusive
    }
}

groupLinesIntoShifts :: Array Line -> Either String (Array Shift)
groupLinesIntoShifts lines = foldM f List.Nil lines <#> List.toUnfoldable <#> Array.reverse
    where
        f :: List Shift -> Line -> Either String (List Shift)
        f xs {date: Date {year, month, day, hour, minute}, entry: BeginsShift id} =
            pure $ {year, month, day, id, sleeps: []} : xs
        f List.Nil line = Left $ "expected BeginsShift; got " <> show line
        f (List.Cons x xs) {date: Date {year, month, day, hour, minute}, entry: FallsAsleep} =
            pure $ x { sleeps = Array.snoc x.sleeps {startMinute:minute, endMinute:60} } : xs
        f (List.Cons x xs) line@{date: Date {year, month, day, hour, minute}, entry: WakesUp} =
            case Array.unsnoc x.sleeps of
                Just {init, last} -> pure $ x { sleeps = Array.snoc init (last { endMinute = minute }) } : xs
                Nothing -> Left $ "got WakesUp without FallsAsleep: " <> show line <> " in " <> show x

sumSleepTimesPerGuard :: Array Shift -> HashMap GuardId Minute
sumSleepTimesPerGuard = foldl f HashMap.empty
    where
        f :: HashMap GuardId Minute -> Shift -> HashMap GuardId Minute
        f prev {id, sleeps} =
            let time = sum $ sleeps <#> (_.endMinute - _.startMinute) in
                HashMap.alter (fromMaybe 0 >>> (_+time) >>> Just) id prev

maximumKeyByValue :: forall k v. Ord v => HashMap k v -> Maybe k
maximumKeyByValue = HashMap.toArrayBy Tuple >>> maximumBy (comparing snd) >=> pure <<< fst

sleepiestGuard :: HashMap GuardId Minute -> Either String GuardId
sleepiestGuard = maximumKeyByValue >>> maybe (Left "no guards") Right

type Histogram a = HashMap a Int

emptyHistogram :: forall a . Histogram a
emptyHistogram = HashMap.empty

updateHistogram :: forall a. Hashable a => a -> Histogram a -> Histogram a
updateHistogram = HashMap.alter (fromMaybe 0 >>> (_+1) >>> Just)

updateHistogramWithSamples :: forall f a. Foldable f => Hashable a => f a -> Histogram a -> Histogram a
updateHistogramWithSamples samples histogram = foldl (flip updateHistogram) histogram samples 

sleepsPerMinutePerGuard :: Array Shift -> HashMap GuardId (Histogram Minute)
sleepsPerMinutePerGuard = foldl f HashMap.empty
    where
        f :: HashMap GuardId (Histogram Minute) -> Shift -> HashMap GuardId (Histogram Minute)
        f prev {id, sleeps} =
            HashMap.alter (fromMaybe emptyHistogram >>> updateHistogramWithSleeps sleeps >>> Just) id prev
        
        updateHistogramWithSleeps :: Array { startMinute :: Minute, endMinute :: Minute } ->  Histogram Minute -> Histogram Minute
        updateHistogramWithSleeps sleeps sleepsPerMinute = foldl (flip updateHistogramWithRange) sleepsPerMinute sleeps

        updateHistogramWithRange :: { startMinute :: Minute, endMinute :: Minute } -> Histogram Minute -> Histogram Minute
        updateHistogramWithRange {startMinute, endMinute} = updateHistogramWithSamples (Array.range startMinute (endMinute-1))

sleepiestMinutePerGuard :: HashMap GuardId (Histogram Minute) -> HashMap GuardId (Maybe Minute)
sleepiestMinutePerGuard = map maximumKeyByValue

strategy1 :: Array Shift -> Either String { guardId :: GuardId, minute :: Minute }
strategy1 shifts =
    let sstpg = sumSleepTimesPerGuard shifts in
    let spmpg = sleepsPerMinutePerGuard shifts in
    do
        guardId <- sleepiestGuard sstpg
        minute <- sleepiestMinutePerGuard spmpg # HashMap.lookup guardId # join # maybe (Left "no minutes") Right
        pure { guardId, minute }