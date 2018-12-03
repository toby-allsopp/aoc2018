module Day1 where

import Prelude

import Data.Foldable (class Foldable, elem, sum)
import Data.HashSet as HS
import Data.Int (fromString)
import Data.Lazy (defer)
import Data.List as L
import Data.List.Lazy as LL
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe, fromJust)
import Data.NonEmpty (NonEmpty(..))
import Data.String.Common (trim)
import Data.String.Utils (lines)
import Data.Traversable (sequence)
import Partial.Unsafe (unsafePartial)

parseChange :: String -> Maybe Int
parseChange = fromString

parseInput :: String -> Maybe (Array Int)
parseInput = sequence <<< map parseChange <<< lines <<< trim

sumFrequencyChanges :: String -> Maybe Int
sumFrequencyChanges = map sum <<< parseInput

lazyScanl :: forall a b. (b -> a -> b) -> b -> LL.List a -> LL.List b
lazyScanl f init l = go init (LL.step l)
    where
        go curr LL.Nil = LL.nil
        go curr (LL.Cons x xs) =
            let next = f curr x in
                LL.List (defer (\_ -> LL.Cons next (go next (LL.step xs))))

-- [1,2,3] -> [[1], [2,1], [3,2,1]]
subLists :: forall a. LL.List a -> LL.List {x :: a, seen :: HS.HashSet a}
subLists = lazyScanl (step) HS.empty
    where
        step {x:prev, seen} x = {x, seen:HS.insert prev seen}

repeats :: forall a. Eq a => LL.List a -> LL.List a
repeats =
    subLists
    >>> LL.filter (\(NEL.NonEmptyList (NonEmpty x seen)) -> x `elem` seen)
    >>> map NEL.head

firstRepeat :: forall a. Eq a => LL.List a -> Maybe a
firstRepeat = repeats >>> LL.head

firstRepeatedCyclicPartialSum :: forall f a. Foldable f => Semiring a => Eq a => f a -> Maybe a
firstRepeatedCyclicPartialSum =
    LL.fromFoldable
    >>> LL.cycle
    >>> lazyScanl (+) zero
    >>> LL.cons zero -- scanl doesn't include the initial value but we need it
    >>> firstRepeat

firstRepeatedFrequencyChange :: String -> Maybe Int
firstRepeatedFrequencyChange = parseInput >=> firstRepeatedCyclicPartialSum