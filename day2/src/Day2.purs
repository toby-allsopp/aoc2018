module Day2 where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NE
import Data.Foldable (any)
import Data.List as List
import Data.String.Yarn (lines, toChars)
import Data.Tuple (Tuple(..), fst, uncurry)

parseInput :: String -> Array String
parseInput = lines

containsExactly :: Int -> String -> Boolean
containsExactly n = toChars >>> Array.group' >>> any (\g -> NE.length g == n)

countContainingExactly :: Int -> Array String -> Int
countContainingExactly n = Array.filter (containsExactly n) >>> Array.length

checksum :: Array String -> Int
checksum ids = (countContainingExactly 2 ids) * (countContainingExactly 3 ids)

allPairs :: forall a. List.List a -> List.List (Tuple a a)
allPairs List.Nil = List.Nil
allPairs (x List.: xs) = map (\y -> Tuple x y) xs <> allPairs xs

something :: Tuple String String -> Array (Tuple Char Char)
something (Tuple s1 s2) = Array.zip (toChars s1) (toChars s2)

compare :: Array String -> Array {comm :: Array Char, diff :: Array (Tuple Char Char)}
compare = List.fromFoldable >>> allPairs >>> Array.fromFoldable >>> map (something >>> Array.partition (uncurry (==)) >>> \{yes, no} -> {comm: map fst yes, diff: no})