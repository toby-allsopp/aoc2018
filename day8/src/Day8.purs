module Day8 (
    Tree,
    parseInput,
    sumMetadata,
    nodeValue
) where

import Parser
import Prelude

import Data.Array as Array
import Data.Maybe (maybe)
import Data.Either (Either, note)
import Data.Foldable (sum)
import Data.Int as Int
import Data.String.Yarn (words)
import Data.Traversable (sequence)
import View as View

data Tree = Tree {
    children :: Array Tree,
    metadata :: Array Int
}

instance treeShow :: Show Tree where
    show (Tree r) = "Tree " <> show r

parseInt :: String -> Either String Int
parseInt s = Int.fromString s # note ("not an integer: " <> s)

parseInput :: String -> Either String Tree
parseInput input = input # words <#> parseInt # sequence >>= doParseTree

doParseTree :: Array Int -> Either String Tree
doParseTree = View.allIndexed >>> runParser (parseTree <* eof)

parseTree :: Parser Int Tree
parseTree = do
    numChildren <- char
    numMetadata <- char
    children <- parseTrees numChildren
    metadata <- take numMetadata
    pure $ Tree { children, metadata: View.toUnfoldable metadata }

parseTrees :: Int -> Parser Int (Array Tree)
parseTrees 0 = pure []
parseTrees n = do
    tree <- parseTree
    trees <- parseTrees (n - 1)
    pure $ Array.cons tree trees

sumMetadata :: Tree -> Int
sumMetadata (Tree { children, metadata }) = sum metadata + sum (sumMetadata <$> children)

nodeValue :: Tree -> Int
nodeValue (Tree { children: [], metadata }) = sum metadata
nodeValue (Tree { children, metadata }) =
    sum $ metadata <#> ((_-1) >>> Array.index children >>> maybe 0 nodeValue)