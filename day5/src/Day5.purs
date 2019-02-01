module Day5 where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Char.Unicode (toLower)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))

isValidUnit :: Char -> Boolean
isValidUnit c = c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z'

parseInput :: String -> Array Char
parseInput = toCharArray >>> Array.filter isValidUnit

doUnitsReact :: Char -> Char -> Boolean
doUnitsReact u1 u2 = u1 /= u2 && toLower u1 == toLower u2

react :: Array Char -> Array Char
react xs = ST.run (STArray.withArray (go 0) xs)
    where
        go :: forall h. Int -> STArray h Char -> ST h Unit
        go i accum = do
            mu1 <- STArray.peek i accum
            mu2 <- STArray.peek (i+1) accum
            case Tuple mu1 mu2 of
                Tuple (Just u1) (Just u2) ->
                    if doUnitsReact u1 u2 then
                        do
                            _ <- STArray.splice i 2 [] accum
                            go (max 0 (i-1)) accum
                    else go (i+1) accum
                _ -> pure unit

