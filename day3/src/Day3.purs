module Day3 where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.MonadZero (class Alt, class Alternative, class MonadZero, class Plus, guard, (<|>))
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as ArrayST
import Data.Char as Char
import Data.Foldable (foldl)
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (trim)
import Data.String as String
import Data.String.Unsafe as String.Unsafe
import Data.String.Yarn (lines)
import Data.Traversable (sequence)
--import Data.String.Utils

data Parser a = Parser (String -> Maybe { result:: a, remaining:: String })

foo :: forall f a b c. Functor f => (a -> f b) -> (b -> c) -> a -> f c 
foo f g = map g <<< f

runParser :: forall a. Parser a -> String -> Maybe a
runParser (Parser run) s = run s <#> (_.result)

instance parserFunctor :: Functor Parser where
    map :: forall a b. (a -> b) -> Parser a -> Parser b
    map f = \(Parser run) -> Parser (\s -> run s <#> (\{result, remaining} -> {result: f result, remaining}))

instance parserApply :: Apply Parser where
    apply :: forall a b. Parser (a -> b) -> Parser a -> Parser b
    apply = ap

instance parserApplicative :: Applicative Parser where
    pure :: forall a. a -> Parser a
    pure x = Parser (\s -> Just {result: x, remaining: s})

instance parserBind :: Bind Parser where
    bind :: forall a b. Parser a -> (a -> Parser b) -> Parser b
    bind (Parser run) f = Parser (run >=> (\{result, remaining} -> f result # (\(Parser runF) -> runF remaining)))

instance parserMonad :: Monad Parser

instance parserAlt :: Alt Parser where
    alt :: forall a. Parser a -> Parser a -> Parser a
    alt (Parser run1) (Parser run2) = Parser (\s -> run1 s <|> run2 s)

instance parserPlus :: Plus Parser where
    empty :: forall a. Parser a
    empty = Parser (\s -> Nothing)

instance parserAlternative :: Alternative Parser
instance parserMonadZero :: MonadZero Parser

peek :: Parser String
peek = Parser (\s -> Just {result: s, remaining: s})

poke :: forall a. a -> String -> Parser a
poke result remaining = Parser (\_ -> Just {result, remaining})

take :: Int -> Parser String
take n = do
    s <- peek
    let {before, after} = String.splitAt n s
    guard (String.length before == n)
    poke before after

literal :: String -> Parser String
literal s = do
    token <- take (String.length s)
    guard (token == s)
    pure token

char :: Parser Char
char = do
    c <- take 1
    pure (String.Unsafe.char c)

digit :: Parser Int
digit = do
    c <- char
    guard (c >= '0' && c <= '9')
    pure (Char.toCharCode c - Char.toCharCode '0')

integer :: Parser Int
integer = go 0 where
    go i = do
        d <- digit
        let n = i*10 + d
        go n <|> pure n

eof :: Parser Unit
eof = do
    s <- peek
    guard (String.null s)
    pure unit

type Rectangle = {
    x :: Int,
    y:: Int,
    width :: Int,
    height :: Int
}
type Claim = {
    id :: Int,
    rect :: Rectangle
}

line :: Parser Claim
line = do
    _ <- literal "#"
    id <- integer
    _ <- literal " @ "
    x <- integer
    _ <- literal ","
    y <- integer
    _ <- literal ": "
    width <- integer
    _ <- literal "x"
    height <- integer
    eof
    pure {id, rect:{x, y, width, height}}
    
parseLine :: String -> Maybe Claim
parseLine = runParser line

parseInput :: String -> Maybe (Array Claim)
parseInput input = input # trim # lines <#> parseLine # sequence

type Coverage = Array (Array Int)

type MutableCoverage h = STArray h (Array Int)

withCoverage :: forall h. (MutableCoverage h -> ST h Unit) -> ST h Coverage
withCoverage f = ArrayST.withArray f (Array.replicate (1000*1000) [])

covIndex :: Int -> Int -> Int
covIndex x y = x * 1000 + y

for2d :: forall h a. Int -> Int -> Int -> Int -> (Int -> Int -> ST h a) -> ST h Unit
for2d xlo xhi ylo yhi f = ST.for xlo xhi (\x -> ST.for ylo yhi (\y -> f x y))

claimSquare :: forall h. MutableCoverage h -> Int -> Int -> Int -> ST h (Array Int)
claimSquare cov id x y = do
    overlaps <- ArrayST.peek (covIndex x y) cov <#> fromMaybe []
    _ <- ArrayST.poke (covIndex x y) (Array.snoc overlaps id) cov
    pure overlaps

claimRectangle :: forall h. MutableCoverage h -> Claim -> ST h Unit
claimRectangle cov {id, rect} = do
    for2d rect.x (rect.x+rect.width) rect.y (rect.y+rect.height) (claimSquare cov id)

claimRectangles :: Array Claim -> Coverage
claimRectangles rects = ST.run (withCoverage (ST.foreach rects <<< claimRectangle))

countOverclaimedElements :: Coverage -> Int
countOverclaimedElements = foldl (\c n -> if (Array.length n) > 1 then c + 1 else c) 0

uncontestedClaims :: Array Claim -> Array Int
uncontestedClaims claims =
    claimRectangles claims
    # foldl (\uncontested ids -> if (Array.length ids) > 1 then HashSet.difference uncontested (HashSet.fromFoldable ids) else uncontested) initialUncontested
    # HashSet.toArray
    where
        initialUncontested :: HashSet Int
        initialUncontested = HashSet.fromFoldable (claims <#> (_.id))
