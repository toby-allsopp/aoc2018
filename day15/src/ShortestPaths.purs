module ShortestPaths where

import Prelude

import Array2d
import Debug (debug)
import Position
import Profile

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Foldable (elem, foldl)
import Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Traversable (class Traversable, sequence, traverse, sequenceDefault, traverse_)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)

type Label = { distance :: Int, next :: HashSet Position }

type Labels h = Array2d (STRef h (Maybe Label))

updatedLabel :: Position -> Int -> Maybe Label -> Maybe Label
updatedLabel from distance = case _ of
    Just l | l.distance < distance  -> Nothing
    Just l | l.distance == distance -> Just $ l { next = HashSet.insert from l.next }
    Just l | otherwise              -> Just { distance, next: HashSet.singleton from }
    Nothing                         -> Just { distance, next: HashSet.singleton from }

updateLabel :: forall h. Partial => Position -> HashSet Position -> Int -> Labels h -> STRef h (Maybe Int) -> Position -> ST h Boolean
updateLabel from tos distance labels upperBoundVar pos = do
    upperBoundM <- STRef.read upperBoundVar
    case upperBoundM of
        Just upperBound | distance > upperBound -> pure false
        _ -> do
            let labelVar = index2d pos labels # fromJust
            oldLabel <- STRef.read labelVar
            case updatedLabel from distance oldLabel of
                newLabel@(Just nl) -> do
                    -- debug ("updateLabel " <> show pos <> ": " <> show oldLabel <> " -> " <> show newLabel) $ \_ -> do
                    void $ STRef.write newLabel labelVar
                    when (HashSet.member pos tos) $ void $ STRef.write (Just distance) upperBoundVar
                    pure $ (Just nl.distance) /= (oldLabel <#> _.distance)
                Nothing -> pure false

labelFrom :: forall h. Partial => Position -> HashSet Position -> Int -> Array Position -> Labels h -> STRef h (Maybe Int) -> ST h (Array Position)
labelFrom from tos distance adjacents labels upperBoundVar =
    -- debug ("labelFrom " <> show distance <> " " <> show from) $ \_ ->
    do
        changed <- STArray.empty
        ST.foreach adjacents \pos -> do
            c <- updateLabel from tos (distance + 1) labels upperBoundVar pos
            when c $ void $ STArray.push pos changed
        STArray.unsafeFreeze changed

wibble :: forall r. Partial => (Position -> Array Position) -> Labels r -> STRef r (Maybe Int) -> Position -> HashSet Position -> ST r (Array Position)
wibble adj labels upperBoundVar from tos = do
    let adjacents = adj from
    let labelVar = unsafeIndex2d from labels
    label <- STRef.read labelVar
    labelFrom from tos (fromJust label # _.distance) adjacents labels upperBoundVar

type Todo r = STArray r Position

wobble :: forall r. Partial => (Position -> Array Position) -> HashSet Position -> Labels r -> STRef r (Maybe Int) -> Todo r -> ST r Unit
wobble adj tos labels upperBoundVar todo = tailRecM go unit
    where
    go :: Unit -> ST r (Step Unit Unit)
    go _ = do
        first <- STArray.peek 0 todo
        -- debug ("wobble " <> show first) $ \_ ->
        case first of
            Nothing -> pure $ Done unit
            Just from -> do
                adjacents <- wibble adj labels upperBoundVar from tos
                _ <- STArray.splice 0 1 [] todo
                _ <- STArray.pushAll adjacents todo
                pure $ Loop unit

fribble :: forall r. Int -> Int -> (Position -> Array Position) -> Position -> HashSet Position -> ST r (Array2d (Maybe Label))
fribble numCols numRows adj from tos = do
    labels <- newArray2dST numCols numRows Nothing
    upperBoundVar <- STRef.new Nothing
    case index2d from labels of
        Just fromLabelVar -> do
            _ <- STRef.write (Just { distance: 0, next: HashSet.empty }) fromLabelVar
            todo <- STArray.thaw [from]
            unsafePartial $ wobble adj tos labels upperBoundVar todo
        Nothing -> pure unit
    freezeArray2d labels

labelShortestPathsTo :: Int -> Int -> (Position -> Array Position) -> Position -> Array Position -> Array2d (Maybe Label)
labelShortestPathsTo numCols numRows adj from tos = ST.run (fribble numCols numRows adj from (HashSet.fromFoldable tos))

followPaths :: Partial => Array2d (Maybe Label) -> Position -> Array (Array Position)
followPaths sps = followPaths' []
    where
    followPaths' :: Partial => Array Position -> Position -> Array (Array Position)
    followPaths' seen p =
        if p `elem` seen then
            unsafeCrashWith $ "cycle detected! " <> show (Array.snoc seen p)
        else
            let maybeLabel = fromJust $ index2d p sps in
            let label = fromJust maybeLabel in
            if HashSet.size label.next == 0 then [[p]] else
                let paths = followPaths' (Array.snoc seen p) =<< HashSet.toArray label.next in
                Array.cons p <$> paths

labelToNextLabels :: Array2d (Maybe Label) -> Label -> Array Label
labelToNextLabels sps { next } = Array.catMaybes $ join <$> flip index2d sps <$> HashSet.toArray next

followNextsUntilDistance :: Int -> Array2d (Maybe Label) -> Position -> Array Position
followNextsUntilDistance d sps from =
    case index2d from sps of
        Nothing -> []
        Just Nothing -> []
        Just (Just label) -> go [from] label.distance
    where
    go froms distance =
        -- debug ("followNextsUntilDistance " <> show d <> " " <> show distance <> " " <> show froms) $ \_ ->
        if distance <= d then froms
        else
            let labels = Array.catMaybes $ join <$> flip index2d sps <$> froms in
            let (nextPositions :: Array Position) = HashSet.toArray $ HashSet.fromFoldable $ join $ HashSet.toArray <$> _.next <$> labels in
            go nextPositions (distance - 1)

type Path = { distance :: Int, paths :: Array (Array Position) }

shortestPathsFrom :: Position -> Array2d (Maybe Label) -> Maybe Path
shortestPathsFrom p sps = do
    maybeLabel <- index2d p sps
    label <- maybeLabel
    let paths = unsafePartial $ followPaths sps p
    pure { distance: label.distance, paths }
