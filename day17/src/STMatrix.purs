module STMatrix where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST (ST, kind Region)
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (sequence, traverse)
import Matrix (Matrix)
import Matrix as M
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

newtype STMatrix r a = STMatrix {
    width :: Int,
    height :: Int,
    values :: STArray r a
}

width :: forall r a. STMatrix r a -> Int
width (STMatrix m) = m.width

height :: forall r a. STMatrix r a -> Int
height (STMatrix m) = m.height

new :: forall r a. Int -> Int -> a -> ST r (STMatrix r a)
new width height init = do
    if width <= 0 then unsafeCrashWith $ "invalid width " <> show width else pure unit
    if height <= 0 then unsafeCrashWith $ "invalid height " <> show height else pure unit
    values <- STArray.unsafeThaw $ Array.replicate (width * height) init
    pure $ STMatrix { width, height, values }

getRow :: forall r a. Int -> STMatrix r a -> ST r (Maybe (Array a))
getRow y m =
    sequence <$> traverse (\x -> get x y m) (Array.range 0 (width m - 1))

freeze :: forall r a. STMatrix r a -> ST r (Matrix a)
freeze m = do
    rows <- sequence <$> traverse (\y -> getRow y m) (Array.range 0 (height m - 1))
    pure $ unsafePartial $ fromJust $ M.fromArray =<< rows

get :: forall r a. Int -> Int -> STMatrix r a -> ST r (Maybe a)
get x y (STMatrix m) =
    if x < 0 || x >= m.width || y < 0 || y >= m.height then pure Nothing else do
    STArray.peek (x + y * m.width) m.values

set :: forall r a. Int -> Int -> a -> STMatrix r a -> ST r (Maybe a)
set x y value = modify x y (const value)

modify :: forall r a. Int -> Int -> (a -> a) -> STMatrix r a -> ST r (Maybe a)
modify x y f (STMatrix m) =
    if x < 0 || x >= m.width || y < 0 || y >= m.height then pure Nothing else
    do
        let i = (x + y * m.width)
        mold <- STArray.peek i m.values
        case mold of
            Just old -> void $ STArray.poke i (f old) m.values
            Nothing -> pure unit
        pure mold
