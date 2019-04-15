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
updatedLabel from distance = case _ of
    Just l | l.distance < distance  -> l
    Just l | l.distance == distance -> l { next = HashSet.insert from l.next }
    Just l | otherwise              -> { distance, next: HashSet.singleton from }
    Nothing                         -> { distance, next: HashSet.singleton from }

updateLabel :: forall h. Partial => Position -> Int -> Labels h -> Position -> ST h Boolean
updateLabel from distance labels pos = do
    let labelVar = index2d pos labels # fromJust
    oldLabel <- STRef.read labelVar
    let newLabel = Just $ updatedLabel from distance oldLabel
    void $ STRef.write newLabel labelVar
    pure $ newLabel /= oldLabel

labelFrom :: forall h. Partial => Position -> Int -> Array Position -> Labels h -> ST h (Array Position)
labelFrom from distance adjacents labels =
    debug ("labelFrom " <> show distance <> " " <> show from) $ \_ -> do
    label <- index2d from labels # fromJust # STRef.read
    changed <- traverse (\pos -> updateLabel from (distance + 1) labels pos <#> if _ then Just pos else Nothing) adjacents
    pure $ Array.catMaybes changed

wibble :: forall r. Partial => (Position -> Array Position) -> Labels r -> Position -> ST r (Array Position)
wibble adj labels from = do
    let adjacents = adj from
    let labelVar = unsafeIndex2d from labels
    label <- STRef.read labelVar
    labelFrom from (fromJust label # _.distance) adjacents labels

type Todo r = STArray r Position

wobble :: forall r. Partial => (Position -> Array Position) -> Labels r -> Todo r -> ST r Unit
wobble adj labels todo = debug ("wobble") $ \_ -> tailRecM go unit
    where
    go :: Unit -> ST r (Step Unit Unit)
    go _ = do
        first <- STArray.peek 0 todo
        debug ("wobble " <> show first) $ \_ ->
        case first of
            Nothing -> pure $ Done unit
            Just from -> do
                adjacents <- wibble adj labels from
                _ <- STArray.splice 0 1 [] todo
                _ <- STArray.pushAll adjacents todo
                pure $ Loop unit

fribble :: forall r. Int -> Int -> (Position -> Array Position) -> Position -> ST r (Array2d (Maybe Label))
fribble numCols numRows adj from = do
    labels <- newArray2dST numCols numRows Nothing
    case index2d from labels of
        Just fromLabelVar -> do
            _ <- STRef.write (Just { distance: 0, next: HashSet.empty }) fromLabelVar
            todo <- STArray.thaw [from]
            unsafePartial $ wobble adj labels todo
        Nothing -> pure unit
    freezeArray2d labels

labelShortestPathsTo :: Int -> Int -> (Position -> Array Position) -> Position -> Array2d (Maybe Label)
labelShortestPathsTo numCols numRows adj from = ST.run (fribble numCols numRows adj from)

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

type Path = { distance :: Int, paths :: Array (Array Position) }

shortestPathsFrom :: Position -> Array2d (Maybe Label) -> Maybe Path
shortestPathsFrom p sps = do
    maybeLabel <- index2d p sps
    label <- maybeLabel
    let paths = unsafePartial $ followPaths sps p
    pure { distance: label.distance, paths }
