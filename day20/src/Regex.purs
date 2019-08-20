module Regex where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (fold)
import Data.Either (Either)
import Data.Foldable (foldl, intercalate)
import Parser as P

data Dir = N | S | E | W

derive instance eqDir :: Eq Dir

instance showDir :: Show Dir where
    show N = "N"
    show S = "S"
    show E = "E"
    show W = "W"

data Regex
    = Atom Dir
    | Sequence (Array Regex)
    | Branch (Array Regex)

derive instance eqRegex :: Eq Regex

instance showRegex :: Show Regex where
    show (Atom d) = show d
    show (Sequence rs) = "(Sequence " <> show rs <> ")"
    show (Branch rs) = "(Branch " <> show rs <> ")"

-- Pretty Printing

ppRegex :: Regex -> String
ppRegex (Atom d) = show d
ppRegex (Sequence rs) = fold (ppRegex <$> rs)
ppRegex (Branch rs) = "(" <> intercalate "|" (ppRegex <$> rs) <> ")"

-- Parsing

type Parser = P.Parser String

parse :: String -> Either String Regex
parse = P.runParser regexParser

regexParser :: Parser Regex
regexParser = do
    _ <- P.literal "^"
    r <- fix \p -> Sequence <$> (P.zeroOrMore (branchOrAtomParser p))
    _ <- P.literal "$"
    pure r

branchOrAtomParser :: Parser Regex -> Parser Regex
branchOrAtomParser p = choiceParser p <|> atomParser

dirParser :: Parser Dir
dirParser = do
    c <- P.char
    case c of
        'N' -> pure N
        'S' -> pure S
        'E' -> pure E
        'W' -> pure W
        _ -> P.fail $ "Expected direction, got '" <> show c <> "'"

atomParser :: Parser Regex
atomParser = Atom <$> dirParser

choiceParser :: Parser Regex -> Parser Regex
choiceParser p = do
    _ <- P.literal "("
    rs <- P.separatedList (P.literal "|") p
    _ <- P.literal ")"
    pure (Branch rs)

-- Folding

foldRegex :: forall a.
    { atom :: a-> Dir -> a
    , sequence :: a -> Regex -> a
    , branch :: a-> Regex -> a
    } -> Regex -> a -> a
foldRegex {atom}     (Atom d)      x = atom x d
foldRegex {sequence} (Sequence rs) x = foldl sequence x rs
foldRegex {branch}   (Branch rs)   x = foldl branch x rs
