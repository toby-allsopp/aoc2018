module Parser where

import Prelude

import Control.MonadZero (class Alt, class Alternative, class MonadZero, class Plus, (<|>))
import Data.Char as Char
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import View (class View)
import View as View

data Parser f c a = Parser (f -> Either String { result:: a, remaining:: f })

runParser :: forall f c a. Parser f c a -> f -> Either String a
runParser (Parser run) s = run s <#> (_.result)

instance parserFunctor :: Functor (Parser f c) where
    map :: forall a b. (a -> b) -> Parser f c a -> Parser f c b
    map f = \(Parser run) -> Parser (\s -> run s <#> (\{result, remaining} -> {result: f result, remaining}))

instance parserApply :: Apply (Parser f c) where
    apply :: forall a b. Parser f c (a -> b) -> Parser f c a -> Parser f c b
    apply = ap

instance parserApplicative :: Applicative (Parser f c) where
    pure :: forall a. a -> Parser f c a
    pure x = Parser (\s -> Right {result: x, remaining: s})

instance parserBind :: Bind (Parser f c) where
    bind :: forall a b. Parser f c a -> (a -> Parser f c b) -> Parser f c b
    bind (Parser run) f = Parser (run >=> (\{result, remaining} -> f result # (\(Parser runF) -> runF remaining)))

instance parserMonad :: Monad (Parser f c)

instance parserAlt :: Alt (Parser f c) where
    alt :: forall a. Parser f c a -> Parser f c a -> Parser f c a
    alt (Parser run1) (Parser run2) = Parser (\s -> run1 s <|> run2 s)

instance parserPlus :: Plus (Parser f c) where
    empty :: forall a. Parser f c a
    empty = Parser (\s -> Left "empty")

instance parserAlternative :: Alternative (Parser f c)
instance parserMonadZero :: MonadZero (Parser f c)

fail :: forall f c a. Show f => String -> Parser f c a
fail message = Parser (\s -> Left $ message <> ": remaining: " <> (show s))

peek :: forall f c. Parser f c f
peek = Parser (\s -> Right {result: s, remaining: s})

poke :: forall f c a. a -> f -> Parser f c a
poke result remaining = Parser (\_ -> Right {result, remaining})

take :: forall f c. View f c => Show f => Int -> Parser f c f
take 0 = pure View.empty
take n = do
    s <- peek
    case View.splitAt n s of
        Just {before, after} -> poke before after
        Nothing -> fail $ "expected " <> show n <> " chars; got " <> show (View.length s)

viewEq :: forall f g a. View f a => View g a => Eq a => f -> g -> Boolean
viewEq v1 v2 =
    case Tuple (View.head v1) (View.head v2) of
        Tuple Nothing Nothing -> true
        Tuple (Just h1) (Just h2) -> if h1.head == h2.head then viewEq h1.tail h2.tail else false
        _ -> false

literal :: forall f g c. View f c => View g c => Eq c => Show f => Show g => g -> Parser f c f
literal s = do
    token <- take (View.length s)
    unless (viewEq token s) $ fail $ "expected " <> (show s) <> "; got " <> (show token)
    pure token

char :: forall f c. View f c => Show f => Parser f c c
char = do
    c <- take 1
    pure $ unsafePartial $ fromJust $ View.head c <#> (_.head)

digit :: forall f. View f Char => Show f => Parser f Char Int
digit = do
    c <- char
    unless (c >= '0' && c <= '9') $ fail $ "expected digit; got " <> show c
    pure (Char.toCharCode c - Char.toCharCode '0')

integer ::forall f. View f Char => Show f => Parser f Char Int
integer = go 0 where
    go i = do
        d <- digit
        let n = i*10 + d
        go n <|> pure n

eof :: forall f c. View f c => Show f => Parser f c Unit
eof = do
    s <- peek
    unless (View.null s) $ fail $ "expected end of string; got " <> (show s)
    pure unit
