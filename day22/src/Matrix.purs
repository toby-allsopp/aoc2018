module Matrix where

import Prelude

import Data.Array (foldMap, foldl, foldr)
import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))

newtype Matrix a = Matrix {
    width :: Int,
    height :: Int,
    values :: Array a
}

instance showMatrix :: Show a => Show (Matrix a) where
    show (Matrix m) = show m

instance functorMatrix :: Functor Matrix where
    map f (Matrix m) = Matrix $ m { values = map f m.values }

instance foldableMatrix :: Foldable Matrix where
    foldl f init (Matrix m) = foldl f init m.values
    foldr f init (Matrix m) = foldr f init m.values
    foldMap f (Matrix m) = foldMap f m.values

fromArray :: forall a. Array (Array a) -> Maybe (Matrix a)
fromArray rows =
    let height = Array.length rows in
    case Array.nub $ Array.sort $ Array.length <$> rows of
        [] -> Nothing
        [width] ->
            let values = join rows in
            Just $ Matrix { width, height, values }
        _ -> Nothing