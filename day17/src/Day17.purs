module Day17 where

import Prelude

import Control.Alt ((<|>))
import Control.MonadZero (guard)
import Control.Monad.Rec.Class ( Step(..), class MonadRec, tailRecM)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (class Foldable, all, any, foldr, foldM)
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.String.Yarn (lines)
import Data.Traversable (traverse, traverse_)
import Matrix (Matrix)
import Matrix as M
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

import Parser as P
import STMatrix as STM
import STMatrix (STMatrix)

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

reach :: forall r. Int -> Int -> STMatrix r Water -> ST r Unit
reach x y w = void $ STM.modify x y markReached w
    where
    markReached None = Reached
    markReached w = w

data Action
    = MoveDown
    | Settle { left :: Int, right :: Int }
    | Spread

reachable :: forall r. Scan -> Int -> Int -> STMatrix r Water -> ST r Boolean
reachable scan x y water = do
    w <- STM.get x y water
    pure case M.get x y scan, w of
        Just Sand, Just None -> true
        Just Sand, Just Reached -> true
        Just Sand, Just Settled -> false
        Just Clay, _ -> false
        Nothing, _ -> false
        _, Nothing -> false

canMoveDown :: forall r. Scan -> STMatrix r Water -> Int -> Int -> ST r (Maybe Action)
canMoveDown scan water x y =
    ifM (reachable scan x (y + 1) water) (pure (Just MoveDown)) (pure Nothing)

findM :: forall m a. MonadRec m => (a -> m Boolean) -> Array a -> m (Maybe a)
findM f xs = tailRecM go 0
    where
    go i = case Array.index xs i of
        Nothing -> pure (Done Nothing)
        Just x -> do
            found <- f x
            pure if found then (Done (Just x)) else (Loop (i + 1))

allM :: forall f m a. Foldable f => Monad m => (a -> m Boolean) -> f a -> m Boolean
allM f = foldM (\b x -> f x <#> (&&) b) true

settleable :: forall r. Scan -> STMatrix r Water -> Int -> Int -> ST r (Maybe Action)
settleable scan water x y = do
    let unreachableX y' x' = do
            r <- reachable scan x' y' water
            pure (not r)
    mleftBarrier <- findM (unreachableX y) (Array.range (x - 1) (minX scan))
    case mleftBarrier of
        Nothing -> pure Nothing
        Just leftBarrier -> do
            mrightBarrier <- findM (unreachableX y) (Array.range (x + 1) (maxX scan))
            case mrightBarrier of
                Nothing -> pure Nothing
                Just rightBarrier -> do
                    sealed <- allM (unreachableX (y + 1)) (Array.range (leftBarrier + 1) (rightBarrier - 1))
                    if sealed then
                        pure $ Just $ Settle { left: leftBarrier + 1, right: rightBarrier - 1 }
                        else
                            pure Nothing

data Dir = Down | Left | Right

waterAction :: forall r. Scan -> STMatrix r Water -> Int -> Int -> Dir -> ST r Action
waterAction scan water x y dir = do
    cmd <- canMoveDown scan water x y
    case cmd, dir of
        Just a, _ -> pure a
        Nothing, Down -> do
            s <- settleable scan water x y
            case s of
                Just a -> pure a
                Nothing -> pure Spread
        Nothing, _ -> pure Spread

settle :: forall r. Int -> { left :: Int, right :: Int } -> STMatrix r Water -> ST r Unit
settle y { left, right } water =
    traverse_ (\x -> STM.set x y Settled water) (Array.range left right)

waterPath :: forall r. Scan -> Int -> Int -> Dir ->  STMatrix r Water -> ST r Unit
waterPath scan x y dir water = tailRecM go {x, y, dir}
    where
    go {x, y, dir} = do
        reach x y water
        action <- waterAction scan water x y dir
        case action of
            MoveDown -> pure $ Loop {x, y: (y + 1), dir: Down}
            Settle lr -> do
                settle y lr water
                pure $ Loop {x, y: (y - 1), dir: Down}
            Spread ->
                let goLeft water = whenM (reachable scan (x - 1) y water) (waterPath scan (x - 1) y Left water)
                    goRight water = whenM (reachable scan (x + 1) y water) (waterPath scan (x + 1) y Right water)
                in
                case dir of
                    Down -> do
                        goLeft water
                        ifM (reachable scan (x + 1) y water)
                            (pure $ Loop { x: x + 1, y, dir: Right })
                            (pure $ Done unit)
                    Left ->
                        ifM (reachable scan (x - 1) y water)
                            (pure $ Loop { x: x - 1, y, dir })
                            (pure $ Done unit)
                    Right ->
                        ifM (reachable scan (x + 1) y water)
                            (pure $ Loop { x: x + 1, y, dir })
                            (pure $ Done unit)

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
linesToScan lines = ST.run do
    mutableScan <- STM.new (maxX - minX + 1) (maxY - minY + 2) Sand
    traverse_ (addLineToScan mutableScan) lines
    scan <- STM.freeze mutableScan
    pure { minX, minY, scan }
    where
        { minX, maxX, minY, maxY } = foldr (\l m -> { minX: min (l.x1 - 1) m.minX, maxX: max (l.x2 + 1) m.maxX, minY: min l.y1 m.minY, maxY: max l.y1 m.maxY}) { minX: top, maxX: bottom, minY: top, maxY: bottom } lines

        addLineToScan :: forall r. STMatrix r Ground -> Line -> ST r Unit
        addLineToScan s l = traverse_ (setClay s) (positionsInLine l)

        positionsInLine :: Line -> Array { x :: Int, y :: Int }
        positionsInLine l = do
            x <- Array.range (l.x1 - minX) (l.x2 - minX)
            y <- Array.range (l.y1 - minY) (l.y2 - minY)
            pure { x, y }

        setClay :: forall r. STMatrix r Ground -> { x :: Int, y :: Int } -> ST r Unit
        setClay s {x, y} = void $ STM.set x y Clay s <#> (fromJustOrCrashWith $ "out of range in setClay " <> show {x,y})

parseScan :: String -> Either String ParsedScan
parseScan s = parseLines s <#> linesToScan

fillWater :: ParsedScan -> WaterMap
fillWater scan = ST.run do
    mutableWater <- STM.new (M.width scan.scan) (M.height scan.scan) None
    waterPath scan.scan (500 - scan.minX) 0 Down mutableWater
    water <- STM.freeze mutableWater
    pure water

data GroundOrWater = Ground Ground | Water Water

instance showGroundOrWater :: Show GroundOrWater where
    show (Ground g) = show g
    show (Water w) = show w

combineWaterAndScan :: ParsedScan -> WaterMap-> Matrix GroundOrWater
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