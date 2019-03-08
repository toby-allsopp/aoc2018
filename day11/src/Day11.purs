module Day11 where

import Prelude

import Data.Array((..))
import Data.Maybe (fromJust)
import Data.Traversable (sum, maximumBy)
import Partial.Unsafe (unsafePartial)

type Coords = { x :: Int, y :: Int }

rackId :: Coords -> Int
rackId { x } = x + 10

hundredsDigit :: Int -> Int
hundredsDigit x = (x `div` 100) `mod` 10

cellPower :: Int -> Coords -> Int
cellPower s c =
    let r = rackId c in
    (hundredsDigit ((r * c.y + s) * r)) - 5

squarePower :: Int -> Coords -> Int
squarePower s { x, y } =
    sum $ (cellPower s) <$>
        [{x, y:y+0}, {x:x+1, y:y+0}, {x:x+2, y:y+0},
         {x, y:y+1}, {x:x+1, y:y+1}, {x:x+2, y:y+1},
         {x, y:y+2}, {x:x+1, y:y+2}, {x:x+2, y:y+2}]

mostPowerfulSquare :: Int -> { power :: Int, coords :: Coords }
mostPowerfulSquare s = unsafePartial $ fromJust $ maximumBy (comparing _.power) $ (\coords -> { power: squarePower s coords, coords }) <$> do
    x <- 1..297
    y <- 1..297
    pure { x, y }