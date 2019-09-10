module Day23 where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (maximumBy)
import Data.Maybe (Maybe)
import Data.Ord (abs)
import Parser as P

type Parser = P.Parser String

type Pos = { x :: Int, y :: Int, z :: Int }
type Bot = { pos :: Pos, r :: Int }

inputLineParser :: Parser Bot
inputLineParser = do
    _ <- P.literal "pos=<"
    x <- P.integer
    _ <- P.literal ","
    y <- P.integer
    _ <- P.literal ","
    z <- P.integer
    _ <- P.literal ">, r="
    r <- P.integer
    pure { pos: { x, y, z }, r }

parseInput :: String -> Either String (Array Bot)
parseInput = P.runParser (P.separatedList (P.eol) inputLineParser)

strongest :: Array Bot -> Maybe Bot
strongest = maximumBy (comparing _.r)

inRange :: Bot -> Array Bot -> Array Bot
inRange bot = Array.filter (botInRange bot)

botInRange :: Bot -> Bot -> Boolean
botInRange b1 b2 = distance b1.pos b2.pos <= b1.r

distance :: Pos -> Pos -> Int
distance p1 p2 = (abs (p1.x - p2.x)) + (abs (p1.y - p2.y)) + (abs (p1.z - p2.z))