module Day8 (
    Tree,
    parseInput,
    sumMetadata
) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Foldable (sum)
import Data.Int as Int
import Data.String.Yarn (words)
import Data.Traversable (sequence)

data Tree = Tree {
    children :: Array Tree,
    metadata :: Array Int
}

parseInput :: String -> Maybe Tree
parseInput input = input # words <#> Int.fromString # sequence >>= makeTree >>= (\{tree, rest} -> if Array.null rest then Just tree else Nothing)

makeTree :: Array Int -> Maybe { tree :: Tree, rest :: Array Int }
makeTree a =
    case Array.take 2 a of
        [numChildren, numMetadata] -> do
            { trees : children, rest } <- makeTrees numChildren (Array.drop 2 a)
            let metadata = Array.take numMetadata rest
            pure { tree : Tree { children, metadata }, rest: Array.drop numMetadata rest }
        _ -> Nothing

makeTrees :: Int -> Array Int -> Maybe { trees :: Array Tree, rest :: Array Int }
makeTrees 0 a = Just { trees : [], rest : a }
makeTrees n a = do
    {tree, rest} <- makeTree a
    {trees, rest: rest' } <- makeTrees (n-1) rest
    pure { trees : Array.cons tree trees, rest : rest' }

sumMetadata :: Tree -> Int
sumMetadata (Tree { children, metadata }) = sum metadata + sum (sumMetadata <$> children)