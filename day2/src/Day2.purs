module Day2 where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NE
import Data.Foldable (any)
import Data.Function (on)
import Data.List as List
import Data.String.Yarn (fromChars, lines, toChars)
import Data.Tuple (Tuple(..), fst, uncurry)

parseInput :: String -> Array String
parseInput = lines

containsExactly :: Int -> String -> Boolean
containsExactly n = toChars >>> Array.group' >>> any (\g -> NE.length g == n)

countContainingExactly :: Int -> Array String -> Int
countContainingExactly n = Array.filter (containsExactly n) >>> Array.length

checksum :: Array String -> Int
checksum ids = (countContainingExactly 2 ids) * (countContainingExactly 3 ids)

allPairsList :: forall a. List.List a -> List.List (Tuple a a)
allPairsList List.Nil = List.Nil
allPairsList (List.Cons x xs) = map (\y -> Tuple x y) xs <> allPairsList xs

allPairs :: forall a. Array a -> Array (Tuple a a)
allPairs = List.fromFoldable >>> allPairsList >>> Array.fromFoldable

zipChars :: String -> String -> Array (Tuple Char Char)
zipChars = Array.zip `on` toChars

reverseCompose2 :: forall a b c d. (a -> b -> c) -> (c -> d) -> a -> b -> d
reverseCompose2 f2 g x y = g (f2 x y)
--reverseCompose2 = flip $ (<<<) <<< (<<<)

infixr 9 reverseCompose2 as >>>>

type DiffResult = {comm :: Array Char, diff :: Array (Tuple Char Char)}
diffStrings :: String -> String -> DiffResult
diffStrings = zipChars >>>> Array.partition (uncurry (==)) >>> \{yes, no} -> {comm: map fst yes, diff: no}

comparePairs :: Array String -> Array DiffResult
comparePairs = allPairs >>> map (uncurry diffStrings)

differByExactly :: Int -> Array String -> Array DiffResult
differByExactly n = comparePairs >>> Array.filter (_.diff >>> Array.length >>> (_ == n))

lettersInCommonInCorrectBoxIds :: Array String -> Array String
lettersInCommonInCorrectBoxIds = differByExactly 1 >>> map (_.comm) >>> map fromChars