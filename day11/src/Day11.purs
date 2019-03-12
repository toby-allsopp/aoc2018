module Day11 where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array ((!!), (..))
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Maybe (fromJust, fromMaybe)
import Data.Traversable (maximumBy)
import Partial.Unsafe (unsafePartial)

newtype Coords = Coords { x :: Int, y :: Int }

instance showCoords :: Show Coords where
    show (Coords c) = show c

derive instance eqCoords :: Eq Coords

rackId :: Coords -> Int
rackId (Coords { x }) = x + 10

hundredsDigit :: Int -> Int
hundredsDigit x = (x `div` 100) `mod` 10

cellPower :: Int -> Coords -> Int
cellPower s = cellPower'
    where
    cellPower' (Coords c) =
        let r = rackId (Coords c) in
        (hundredsDigit ((r * c.y + s) * r)) - 5

data SummedAreaTable = SummedAreaTable (Array Int)

makeSummedAreaTable :: Array Int -> SummedAreaTable
makeSummedAreaTable values = SummedAreaTable $ ST.run (STArray.withArray go values)
    where
    go :: forall r. STArray r Int -> ST r Unit
    go a = do
        ST.for 0 300 \y ->
            ST.for 0 300 \x -> do
                up <- STArray.peek (coord x (y-1)) a <#> fromMaybe 0
                left <- STArray.peek (coord (x-1) y) a <#> fromMaybe 0
                diag <- STArray.peek (coord (x-1) (y-1)) a <#> fromMaybe 0
                STArray.modify (coord x y) (\i -> i + up + left - diag) a
    coord x y = x + y * 300

area :: Int -> Int -> Int -> Int -> SummedAreaTable -> Int
area x1 y1 x2 y2 (SummedAreaTable table) =
    let capitalI x y = table !! (x + y * 300) # fromMaybe 0 in
    let a = capitalI (x1 - 1) (y1 - 1)
        b = capitalI x2 (y1 - 1)
        c = capitalI (x1 - 1) y2
        d = capitalI x2 y2 in
    d + a - b - c

newtype Square = Square { coords :: Coords, size :: Int }

squarePower :: Square -> SummedAreaTable -> Int
squarePower (Square { coords: Coords { x, y }, size }) =
    area (x - 1) (y - 1) (x + size - 2) (y + size - 2)

type MostPowerfulSquare = { power :: Int, coords :: Coords, size :: Int }

allSquarePowers :: Int -> SummedAreaTable
allSquarePowers s = oneByOnes # makeSummedAreaTable
    where
    oneByOnes :: Array Int
    oneByOnes = do
        y <- 1 .. 300
        x <- 1 .. 300
        pure $ cellPower s (Coords { x, y })

mostPowerfulSquare :: Int -> MostPowerfulSquare
mostPowerfulSquare s = unsafePartial $ fromJust $ maximumBy (comparing _.power) squarePowers
    where
    table = (allSquarePowers s)

    squarePowers :: Array MostPowerfulSquare
    squarePowers = do
        x <- 1 .. 298
        y <- 1 .. 298
        let coords = Coords { x, y }
        pure $ { coords, size: 3, power: squarePower (Square { coords, size: 3 }) table }

mostPowerfulAnySquare :: Int -> MostPowerfulSquare
mostPowerfulAnySquare s = unsafePartial $ fromJust $ maximumBy (comparing _.power) squarePowers
    where
    table = (allSquarePowers s)

    squarePowers :: Array MostPowerfulSquare
    squarePowers = do
        size <- 1 .. 300
        x <- 1 .. (300 - size + 1)
        y <- 1 .. (300 - size + 1)
        let coords = Coords { x, y }
        pure $ { coords, size, power: squarePower (Square { coords, size }) table }
