module Day1 where

import Prelude

import Data.Foldable (class Foldable, sum)
import Data.HashSet as HS
import Data.Hashable (class Hashable)
import Data.Int (fromString)
import Data.Lazy (defer)
import Data.List.Lazy as LL
import Data.Maybe (Maybe)
import Data.String.Common (trim)
import Data.String.Utils (lines)
import Data.Traversable (sequence)

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

-- [1,2,3] -> [{1, []}, {2,[1]}, {3,[2,1]}]
subLists :: forall a. Hashable a => LL.List a -> LL.List {x :: a, seen :: HS.HashSet a}
subLists l = go HS.empty (LL.step l)
    where
        go seen LL.Nil = LL.nil
        go seen (LL.Cons x xs) = LL.List (defer (\_ -> LL.Cons {x, seen} (go (HS.insert x seen) (LL.step xs))))

repeats :: forall a. Hashable a => LL.List a -> LL.List a
repeats =
    subLists
    >>> LL.filter (\{x, seen} -> x `HS.member` seen)
    >>> map _.x

firstRepeat :: forall a. Hashable a => LL.List a -> Maybe a
firstRepeat = repeats >>> LL.head

firstRepeatedCyclicPartialSum :: forall f a. Foldable f => Semiring a => Hashable a => f a -> Maybe a
firstRepeatedCyclicPartialSum =
    LL.fromFoldable
    >>> LL.cycle
    >>> lazyScanl (+) zero
    >>> LL.cons zero -- scanl doesn't include the initial value but we need it
    >>> firstRepeat

firstRepeatedFrequencyChange :: String -> Maybe Int
firstRepeatedFrequencyChange = parseInput >=> firstRepeatedCyclicPartialSum