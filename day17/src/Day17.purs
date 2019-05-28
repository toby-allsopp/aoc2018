module Day17 where

import Prelude

import Control.Alt ((<|>))
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (all, any, foldr, foldM)
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.String.Yarn (lines)
import Data.Traversable (traverse)
import Matrix (Matrix)
import Matrix as M
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

import Parser as P

data Ground = Sand | Clay

instance showGround :: Show Ground where
    show Sand = "."
    show Clay = "#"

type Scan = Matrix Ground

minX :: Scan -> Int
minX scan = 0

maxX :: Scan -> Int
maxX scan = M.width scan - 1

data Water = None | Settled | Reached

instance showWater :: Show Water where
    show None = "."
    show Settled = "~"
    show Reached = "|"

type WaterMap = Matrix Water

fromJustOrCrashWith :: forall a. String -> Maybe a -> a
fromJustOrCrashWith msg =
    case _ of
        Just x -> x
        Nothing -> unsafeCrashWith msg

reach :: Int -> Int -> WaterMap -> WaterMap
reach x y = M.modify x y markReached >>> (fromJustOrCrashWith $ "out of bounds in reach " <> show x <> ", " <> show y)
    where
    markReached None = Reached
    markReached w = w

data Action
    = MoveDown
    | Settle { left :: Int, right :: Int }
    | Spread

reachable :: Scan -> Int -> Int -> WaterMap -> Boolean
reachable scan x y water =
    case M.get x y scan, M.get x y water of
        Just Sand, Just None -> true
        Just Sand, Just Reached -> true
        Just Sand, Just Settled -> false
        Just Clay, _ -> false
        Nothing, _ -> false
        _, Nothing -> false

canMoveDown :: Scan -> WaterMap -> Int -> Int -> Maybe Action
canMoveDown scan water x y =
    if reachable scan x (y + 1) water then Just MoveDown else Nothing

settleable :: Scan -> WaterMap -> Int -> Int -> Maybe Action
settleable scan water x y = do
    let unreachableX y' x' = not $ reachable scan x' y' water
    leftBarrier <- Array.find (unreachableX y) (Array.range (x - 1) (minX scan))
    rightBarrier <- Array.find (unreachableX y) (Array.range (x + 1) (maxX scan))
    guard $ all (unreachableX (y + 1)) (Array.range (leftBarrier + 1) (rightBarrier - 1))
    pure $ Settle { left: leftBarrier + 1, right: rightBarrier - 1 }

waterAction :: Scan -> WaterMap -> Int -> Int -> Action
waterAction scan water x y =
    canMoveDown scan water x y <|> settleable scan water x y # fromMaybe Spread

settle :: Int -> { left :: Int, right :: Int } -> WaterMap -> WaterMap
settle y { left, right } water =
    foldM (\w x -> M.set x y Settled w) water (Array.range left right)
    # (fromJustOrCrashWith $ "out of bounds in settle " <> show y <> " " <> show { left, right })

data Dir = Down | Left | Right

waterPath :: Scan -> Int -> Int -> Dir ->  WaterMap -> WaterMap
waterPath scan x y dir water =
    reach x y water # case waterAction scan water x y of
        MoveDown -> (waterPath scan x (y + 1) Down <<< reach x y)
        Settle lr -> (waterPath scan x (y - 1) Down <<< settle y lr)
        Spread ->
            let goLeft = ifM (reachable scan (x - 1) y) (waterPath scan (x - 1) y Left) identity
                goRight = ifM (reachable scan (x + 1) y) (waterPath scan (x + 1) y Right) identity
            in
            case dir of
                Down -> (goLeft <<< goRight)
                Left -> goLeft
                Right -> goRight

type Parser = P.Parser Char

type Line = {
    x1 :: Int,
    x2 :: Int,
    y1 :: Int,
    y2 :: Int
}

lineParser :: Parser Line
lineParser = xrange <|> yrange <* P.eof
    where
    xrange :: Parser Line
    xrange = do
        _ <- P.literal "y="
        y <- P.integer
        _ <- P.literal ", x="
        x1 <- P.integer
        _ <- P.literal ".."
        x2 <- P.integer
        pure { x1, x2, y1: y, y2: y}
    yrange :: Parser Line
    yrange = do
        _ <- P.literal "x="
        x <- P.integer
        _ <- P.literal ", y="
        y1 <- P.integer
        _ <- P.literal ".."
        y2 <- P.integer
        pure { x1: x, x2: x, y1, y2}

parseLines :: String -> Either String (Array Line)
parseLines = lines >>> traverse (P.runParser lineParser)

type ParsedScan = { minX :: Int, minY :: Int, scan :: Scan }

linesToScan :: Array Line -> ParsedScan
linesToScan lines =
    let emptyScan = M.repeat (maxX - minX + 3) (maxY - minY + 2) Sand in
    { minX, minY, scan: foldr addLineToScan emptyScan lines }
    where
        { minX, maxX, minY, maxY } = foldr (\l m -> { minX: min l.x1 m.minX, maxX: max l.x2 m.maxX, minY: min l.y1 m.minY, maxY: max l.y1 m.maxY}) { minX: top, maxX: bottom, minY: top, maxY: bottom } lines

        addLineToScan :: Line -> Scan -> Scan
        addLineToScan l s = foldr setClay s (positionsInLine l)

        positionsInLine :: Line -> Array { x :: Int, y :: Int }
        positionsInLine l = do
            x <- Array.range (l.x1 - minX) (l.x2 - minX)
            y <- Array.range (l.y1 - minY) (l.y2 - minY)
            pure { x, y }

        setClay :: { x :: Int, y :: Int } -> Scan -> Scan
        setClay {x, y} s = M.set x y Clay s # (fromJustOrCrashWith $ "out of range in setClay " <> show {x,y})

parseScan :: String -> Either String ParsedScan
parseScan s = parseLines s <#> linesToScan

fillWater :: ParsedScan -> WaterMap
fillWater scan =
    let emptyWater = M.repeat (M.width scan.scan) (M.height scan.scan) None in
    waterPath scan.scan (500 - scan.minX) 0 Down emptyWater

data GroundOrWater = Ground Ground | Water Water

instance showGroundOrWater :: Show GroundOrWater where
    show (Ground g) = show g
    show (Water w) = show w

combineWaterAndScan :: ParsedScan -> WaterMap -> Matrix GroundOrWater
combineWaterAndScan scan water =
    M.zipWith f scan.scan water # (fromJustOrCrashWith $ "different dimensions :(")
    where
        f g None = Ground g
        f _ w = Water w

dropLastRow :: forall a. Matrix a -> Matrix a
dropLastRow m =
    let rows = (\y -> unsafePartial $ fromJust $ M.getRow y m) <$> Array.range 0 ((M.height m) - 1) in
    unsafePartial $ fromJust $ M.fromArray $ Array.dropEnd 1 rows

numReachable :: WaterMap -> Int
numReachable water =
    foldr f 0 (dropLastRow water)
    where
    f None s = s
    f _ s = s + 1