module Array2d where

import Prelude

import Position (Position, makePosition, positionX, positionY)

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.ST (ST)

import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Data.Array as Array
import Data.Array ((!!))
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex, foldrWithIndex, foldMapWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (joinWith)
import Data.String.CodeUnits as String
import Data.String.Yarn (lines)
import Data.Traversable (class Traversable, sequence, sequenceDefault, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndexDefault)

newtype Array2d a = Array2d { numCols :: Int, rows :: (Array (Array a)) }

derive instance eqArray2d :: Eq a => Eq (Array2d a)

instance showArray2d :: Show a => Show (Array2d a) where
    show a2d@(Array2d a) = "[" <> joinWith ",\n " (showRow <$> a.rows) <> "]"
        where
        maxWidth :: Int
        maxWidth = fromMaybe 0 $ maximum $ (String.length <<< show) <$> a2d

        showRow :: Array a -> String
        showRow row = "[" <> (joinWith ", " $ padTo maxWidth <$> show <$> row) <> "]"

        padTo :: Int -> String -> String
        padTo w s = s <> String.fromCharArray (Array.replicate (w - String.length s) ' ')

instance functorArray2d :: Functor Array2d where
    map f (Array2d a) = Array2d (a { rows = map (map f) a.rows })

instance foldableArray2d :: Foldable Array2d where
    foldl f init (Array2d a) = foldl (foldl f) init a.rows
    foldr f init (Array2d a) = foldr (flip (foldr f)) init a.rows
    foldMap f (Array2d a) = foldMap (foldMap f) a.rows

instance traversableArray2d :: Traversable Array2d where
    traverse f (Array2d a) = Array2d <$> a { rows = _ } <$> sequence (traverse f <$> a.rows)
    sequence = sequenceDefault

instance functorWithIndexArray2 :: FunctorWithIndex Position Array2d where
    mapWithIndex f (Array2d a) = Array2d (a { rows = mapWithIndex (\y -> mapWithIndex (\x -> f (makePosition x y))) a.rows })

instance foldableWithIndexArray2d :: FoldableWithIndex Position Array2d where
    foldlWithIndex f init (Array2d a) = foldlWithIndex (\y -> foldlWithIndex (\x -> f (makePosition x y))) init a.rows
    foldrWithIndex f init (Array2d a) = foldrWithIndex (\y row result -> foldrWithIndex (\x -> f (makePosition x y)) result row) init a.rows
    foldMapWithIndex f (Array2d a) = foldMapWithIndex (\y -> foldMapWithIndex (\x -> f (makePosition x y))) a.rows

instance traversableWithIndexArray2d :: TraversableWithIndex Position Array2d where
    traverseWithIndex = traverseWithIndexDefault

rowsToArray2d :: forall m a. MonadThrow String m => Array (Array a) -> m (Array2d a)
rowsToArray2d rows =
    let rowLengths = rows <#> Array.length # Array.nub in
    case rowLengths of
        [] -> pure $ Array2d { numCols: 0, rows }
        [numCols] -> pure $ Array2d { numCols, rows }
        _ -> throwError $ "uneven row lengths: " <> show rowLengths

parseArray2d :: forall m. MonadThrow String m => String -> m (Array2d Char)
parseArray2d s =
    let rows = lines s <#> String.toCharArray in
    rowsToArray2d rows

array2dCols :: forall a. Array2d a -> Int
array2dCols (Array2d { numCols }) = numCols

array2dRows :: forall a. Array2d a -> Int
array2dRows (Array2d { rows }) = Array.length rows

index2d :: forall a. Position -> Array2d a -> Maybe a
index2d p (Array2d { rows }) = rows !! (positionY p) >>= (_ !! positionX p)

unsafeIndex2d :: forall a. Partial => Position -> Array2d a -> a
unsafeIndex2d p (Array2d { rows }) = flip Array.unsafeIndex (positionX p) $ flip Array.unsafeIndex (positionY p) rows

newArray2dST :: forall h a. Int -> Int -> a -> ST h (Array2d (STRef h a))
newArray2dST numCols numRows x = do
    rows <- sequence $ Array.replicate numRows $ sequence $ Array.replicate numCols (STRef.new x)
    pure $ Array2d { numCols, rows }

freezeArray2d :: forall h a. Array2d (STRef h a) -> ST h (Array2d a)
freezeArray2d (Array2d { numCols, rows }) = do
    frozenRows <- traverse (traverse STRef.read) rows
    pure $ Array2d { numCols, rows: frozenRows }

