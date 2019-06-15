module Array2d where

import Position
import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Data.Array ((!!))
import Data.Array as Array
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum, minimumBy, sum)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex, foldrWithIndex, foldMapWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Hashable (class Hashable, hash)
import Data.Int (rem, quot)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String (joinWith)
import Data.String.CodeUnits as String
import Data.String.Yarn (lines, unlines)
import Data.Traversable (class Traversable, sequence, traverse, sequenceDefault, traverse_)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex, traverseWithIndexDefault)

newtype Array2d a = Array2d { numCols :: Int, elements :: Array a }

array2dToRows :: forall a. Array2d a -> Array (Array a)
array2dToRows (Array2d a) = go a.numCols a.elements []
    where
    go :: Int -> Array a -> Array (Array a) -> Array (Array a)
    go n [] rows = rows
    go n xs rows = go n (Array.drop n xs) (Array.snoc rows (Array.take n xs))

derive instance eqArray2d :: Eq a => Eq (Array2d a)

instance hashableAeeay2d :: Hashable a => Hashable (Array2d a) where
    hash (Array2d a) = hash a

instance showArray2d :: Show a => Show (Array2d a) where
    show a2d@(Array2d a) = "[" <> joinWith ",\n " (showRow <$> array2dToRows a2d) <> "]"
        where
        maxWidth :: Int
        maxWidth = fromMaybe 0 $ maximum $ (String.length <<< show) <$> a2d

        showRow :: Array a -> String
        showRow row = "[" <> (joinWith ", " $ padTo maxWidth <$> show <$> row) <> "]"

        padTo :: Int -> String -> String
        padTo w s = s <> String.fromCharArray (Array.replicate (w - String.length s) ' ')

instance functorArray2d :: Functor Array2d where
    map f (Array2d a) = Array2d (a { elements = map f a.elements })

instance foldableArray2d :: Foldable Array2d where
    foldl f init (Array2d a) = foldl f init a.elements
    foldr f init (Array2d a) = foldr f init a.elements
    foldMap f (Array2d a) = foldMap f a.elements

instance traversableArray2d :: Traversable Array2d where
    traverse f (Array2d a) = Array2d <$> a { elements = _ } <$> traverse f a.elements
    sequence = sequenceDefault

indexToPosition :: Int -> Int -> Position
indexToPosition numCols i = makePosition (i `rem` numCols) (i `quot` numCols)

positionToIndex :: Int -> Position -> Int
positionToIndex numCols p =
    case positionX p of
        x | x < 0 -> -1
        x | x >= numCols -> -2
        x -> x + (positionY p) * numCols

instance functorWithIndexArray2d :: FunctorWithIndex Position Array2d where
    mapWithIndex f (Array2d a) = Array2d (a { elements = mapWithIndex (\i -> f (indexToPosition a.numCols i)) a.elements })

instance foldableWithIndexArray2d :: FoldableWithIndex Position Array2d where
    foldlWithIndex f init (Array2d a) = foldlWithIndex (\i -> f (indexToPosition a.numCols i)) init a.elements
    foldrWithIndex f init (Array2d a) = foldrWithIndex (\i -> f (indexToPosition a.numCols i)) init a.elements
    foldMapWithIndex f (Array2d a) = foldMapWithIndex (\i -> f (indexToPosition a.numCols i)) a.elements

instance traversableWithIndexArray2d :: TraversableWithIndex Position Array2d where
    traverseWithIndex = traverseWithIndexDefault

rowsToArray2d :: forall m a. MonadThrow String m => Array (Array a) -> m (Array2d a)
rowsToArray2d rows =
    let rowLengths = rows <#> Array.length # Array.nub in
    case rowLengths of
        [] -> pure $ Array2d { numCols: 0, elements: join rows }
        [numCols] -> pure $ Array2d { numCols, elements: join rows }
        _ -> throwError $ "uneven row lengths: " <> show rowLengths

parseArray2d :: forall m. MonadThrow String m => String -> m (Array2d Char)
parseArray2d s =
    let rows = lines s <#> String.toCharArray in
    rowsToArray2d rows

array2dCols :: forall a. Array2d a -> Int
array2dCols (Array2d { numCols }) = numCols

array2dRows :: forall a. Array2d a -> Int
array2dRows (Array2d a) = Array.length a.elements `div` a.numCols

index2d :: forall a. Position -> Array2d a -> Maybe a
index2d p (Array2d a) = a.elements !! (positionToIndex a.numCols p)

unsafeIndex2d :: forall a. Partial => Position -> Array2d a -> a
unsafeIndex2d p (Array2d a) = flip Array.unsafeIndex (positionToIndex a.numCols p) a.elements

newArray2dST :: forall h a. Int -> Int -> a -> ST h (Array2d (STRef h a))
newArray2dST numCols numRows x = do
    elements <- sequence $ Array.replicate (numCols * numRows) (STRef.new x)
    pure $ Array2d { numCols, elements }

freezeArray2d :: forall h a. Array2d (STRef h a) -> ST h (Array2d a)
freezeArray2d (Array2d { numCols, elements }) = do
    frozenElements <- traverse STRef.read elements
    pure $ Array2d { numCols, elements: frozenElements }

