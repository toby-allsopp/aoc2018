module Day18 where

import Array2d (Array2d, index2d)
import Array2d as Array2d
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array (catMaybes)
import Data.Either (Either)
import Data.Foldable (foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Hashable (class Hashable)
import Data.Traversable (traverse)
import Position (Position, makePosition, positionX, positionY)
import Prelude (class Eq, class Show, flip, otherwise, pure, show, zero, (#), ($), (&&), (*), (+), (-), (<#>), (<>), (==), (>=), (>=>))

data AcreContent = Open | Trees | Lumberyard

derive instance eqAcreContent :: Eq AcreContent

instance hashableAcreContent :: Hashable AcreContent where
    hash Open = 1
    hash Trees = 2
    hash Lumberyard = 3

instance showAcreContent :: Show AcreContent where
    show Open = "."
    show Trees = "|"
    show Lumberyard = "#"

charToAcreContent :: forall m. MonadThrow String m => Char -> m AcreContent
charToAcreContent '.' = pure Open
charToAcreContent '|' = pure Trees
charToAcreContent '#' = pure Lumberyard
charToAcreContent c = throwError $ "invalid acre char '" <> show c <> "'"

changedAcre :: { numOpen :: Int, numWooded :: Int, numLumberyard :: Int } -> AcreContent -> AcreContent
changedAcre { numOpen, numWooded, numLumberyard } = case _ of
    Open | numWooded >= 3 -> Trees
    Open | otherwise -> Open
    Trees | numLumberyard >= 3 -> Lumberyard
    Trees | otherwise -> Trees
    Lumberyard | numLumberyard >= 1 && numWooded >= 1 -> Lumberyard
    Lumberyard | otherwise -> Open

type Scan = Array2d AcreContent

adjacentPositions :: Position -> Array Position
adjacentPositions p =
    let x = positionX p
        y = positionY p
    in
        [makePosition (x - 1) (y - 1)
        ,makePosition (x - 1) (y - 0)
        ,makePosition (x - 1) (y + 1)
        ,makePosition (x - 0) (y - 1)
        ,makePosition (x - 0) (y + 1)
        ,makePosition (x + 1) (y - 1)
        ,makePosition (x + 1) (y - 0)
        ,makePosition (x + 1) (y + 1)
        ]

adjacentAcres :: Scan -> Position -> Array AcreContent
adjacentAcres scan p = adjacentPositions p <#> flip index2d scan # catMaybes

adjacentInfo :: Scan -> Position -> { numOpen :: Int, numWooded :: Int, numLumberyard :: Int }
adjacentInfo scan p = adjacentAcres scan p # foldr f zero
    where
        f Open info = info { numOpen = info.numOpen + 1 }
        f Trees info = info { numWooded = info.numWooded + 1 }
        f Lumberyard info = info { numLumberyard = info.numLumberyard + 1 }

changedScan :: Scan -> Scan
changedScan scan =
    mapWithIndex go scan
    where
        go :: Position -> AcreContent -> AcreContent
        go p acre = changedAcre (adjacentInfo scan p) acre

parseScan :: String -> Either String Scan
parseScan = Array2d.parseArray2d >=> traverse charToAcreContent

resourceValue :: Scan -> Int
resourceValue scan = numWooded * numLumberyards
    where
    numWooded = count Trees scan
    numLumberyards = count Lumberyard scan
    count x xs = foldr (\y c -> if y == x then c + 1 else c) 0 xs