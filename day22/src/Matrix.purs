module Matrix where

import Prelude

import Data.Array (foldMap, foldl, foldr)
import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

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

fromRowMajor :: forall a. Int -> Int -> Array a -> Maybe (Matrix a)
fromRowMajor width height values =
    if width * height == Array.length values then
        Just $ Matrix { width, height, values }
    else
        Nothing

fromArray :: forall a. Array (Array a) -> Maybe (Matrix a)
fromArray rows =
    let height = Array.length rows in
    case Array.nub $ Array.sort $ Array.length <$> rows of
        [] -> Nothing
        [width] ->
            let values = join rows in
            Just $ Matrix { width, height, values }
        _ -> Nothing

width :: forall a. Matrix a -> Int
width (Matrix m) = m.width

height :: forall a. Matrix a -> Int
height (Matrix m) = m.height

get :: forall a. Int -> Int -> Matrix a -> Maybe a
get x y (Matrix m) =
    if x < 0 || x >= m.width || y < 0 || y >= m.height then
        Nothing
    else
        Array.index m.values (x + y * m.width)

unsafeGet :: forall a. Partial => Int -> Int -> Matrix a -> a
unsafeGet x y (Matrix m) =
    if x < 0 || x >= m.width || y < 0 || y >= m.height then
        unsafeCrashWith $ "out of range: " <> show x <> ", " <> show y
    else
        Array.unsafeIndex m.values (x + y * m.width)

submatrix :: forall a. Int -> Int -> Int -> Int -> Matrix a -> Matrix a
submatrix x0 y0 x1 y1 m =
    unsafePartial $ fromJust $ fromArray $
        Array.range y0 (y1 - 1) <#> \y ->
            Array.range x0 (x1 - 1) <#> \x ->
                unsafeGet x y m