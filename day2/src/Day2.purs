module Day2 where

import Prelude

import Data.Array (filter, group', length)
import Data.Array.NonEmpty as NE
import Data.Foldable (any)
import Data.String.Yarn (lines, toChars)

parseInput :: String -> Array String
parseInput = lines

containsExactly :: Int -> String -> Boolean
containsExactly n = toChars >>> group' >>> any (\g -> NE.length g == n)

countContainingExactly :: Int -> Array String -> Int
countContainingExactly n = filter (containsExactly n) >>> length

checksum :: Array String -> Int
checksum ids = (countContainingExactly 2 ids) * (countContainingExactly 3 ids)