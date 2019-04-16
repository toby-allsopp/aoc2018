module ShortestPaths where

import Prelude

import Array2d
import Debug (debug)
import Position

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Foldable (elem)
import Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Traversable (class Traversable, sequence, traverse, sequenceDefault, traverse_)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)

type Label = { distance :: Int, next :: HashSet Position }

type Labels h = Array2d (STRef h (Maybe Label))

updatedLabel :: Position -> Int -> Maybe Label -> Label
updatedLabel to distance = case _ of
    Just l | l.distance < distance  -> l
    Just l | l.distance == distance -> l { next = HashSet.insert to l.next }
    Just l | otherwise              -> { distance, next: HashSet.singleton to }
    Nothing                         -> { distance, next: HashSet.singleton to }

updateLabel :: forall h. Partial => Position -> Int -> Labels h -> Position -> ST h Boolean
updateLabel to distance labels pos = do
    let labelVar = index2d pos labels # fromJust
    oldLabel <- STRef.read labelVar
    let newLabel = Just $ updatedLabel to distance oldLabel
    void $ STRef.write newLabel labelVar
    pure $ newLabel /= oldLabel

labelTo :: forall h. Partial => Position -> Int -> Array Position -> Labels h -> Position -> ST h (Array Position)
labelTo to distance adjacents labels from = do
    upperBoundM <- index2d from labels # fromJust # STRef.read
    debug ("labelTo " <> show to <> " " <> show distance <> " " <> show from <> " " <> show upperBoundM) $ \_ ->
    case upperBoundM of
        Just upperBound | upperBound.distance < (distance + manhattanDistance to from)-> pure []
        _ -> do
            label <- index2d to labels # fromJust # STRef.read
            changed <- traverse (\pos -> updateLabel to (distance + 1) labels pos <#> if _ then Just pos else Nothing) adjacents
            pure $ Array.catMaybes changed

wibble :: forall r. Partial => (Position -> Array Position) -> Labels r -> Position -> Position -> ST r (Array Position)
wibble adj labels from to = do
    let adjacents = adj to
    let labelVar = unsafeIndex2d to labels
    label <- STRef.read labelVar
    labelTo to (fromJust label # _.distance) adjacents labels from

type Todo r = STArray r Position

wobble :: forall r. Partial => (Position -> Array Position) -> Labels r -> Todo r -> Position -> ST r Unit
wobble adj labels todo from = debug ("wobble " <> show from) $ \_ -> tailRecM go unit
    where
    go :: Unit -> ST r (Step Unit Unit)
    go _ = do
        first <- STArray.peek 0 todo
        debug ("wobble " <> show first) $ \_ ->
        case first of
            Nothing -> pure $ Done unit
            Just to -> do
                adjacents <- wibble adj labels from to
                _ <- STArray.splice 0 1 adjacents todo
                -- _ <- STArray.pushAll adjacents todo
                pure $ Loop unit

fribble :: forall r. Int -> Int -> (Position -> Array Position) -> Position -> Position -> ST r (Maybe Label)
fribble numCols numRows adj from to = do
    labels <- newArray2dST numCols numRows Nothing
    case index2d to labels of
        Just toLabelVar -> do
            _ <- STRef.write (Just { distance: 0, next: HashSet.empty }) toLabelVar
            todo <- STArray.thaw [to]
            unsafePartial $ wobble adj labels todo from
        Nothing -> pure unit
    case index2d from labels of
        Just fromLabelVar -> STRef.read fromLabelVar
        Nothing -> pure Nothing

shortestPathsTo :: Int -> Int -> (Position -> Array Position) -> Position -> Position -> Maybe Label
shortestPathsTo numCols numRows adj from to = ST.run (fribble numCols numRows adj from to)

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

type Path = { distance :: Int, nexts :: Array Position }

labelToPath :: Label -> Path
labelToPath label = { distance: label.distance, nexts: HashSet.toArray label.next }

shortestPathsFrom :: Position -> Array2d (Maybe Label) -> Maybe Path
shortestPathsFrom p sps = do
    maybeLabel <- index2d p sps
    label <- maybeLabel
    -- let paths = unsafePartial $ followPaths sps p
    pure { distance: label.distance, nexts: HashSet.toArray label.next }
