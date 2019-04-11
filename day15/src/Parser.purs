module Parser where

import Prelude

import Control.MonadZero (class Alt, class Alternative, class MonadZero, class Plus, guard, (<|>))
import Data.Array as Array
import Data.Char as Char
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import View (class View, class Viewable, AnyView)
import View as View

data Parser c a = Parser (forall v. View v c => v -> Either String { result:: a, remaining:: AnyView c })

runParser :: forall f v c a. Viewable f v c => Parser c a -> f -> Either String a
runParser (Parser run) s = run (View.view s) <#> (_.result)

instance parserFunctor :: Functor (Parser c) where
    map :: forall a b. (a -> b) -> Parser c a -> Parser c b
    map f = \(Parser run) -> Parser (\s -> run s <#> (\{result, remaining} -> {result: f result, remaining}))

instance parserApply :: Apply (Parser c) where
    apply :: forall a b. Parser c (a -> b) -> Parser c a -> Parser c b
    apply = ap

instance parserApplicative :: Applicative (Parser c) where
    pure :: forall a. a -> Parser c a
    pure x = Parser (\s -> Right {result: x, remaining: View.eraseView s})

instance parserBind :: Bind (Parser c) where
    bind :: forall a b. Parser c a -> (a -> Parser c b) -> Parser c b
    bind (Parser run) f = Parser (run >=> (\{result, remaining} -> f result # (\(Parser runF) -> View.uneraseView runF remaining)))

instance parserMonad :: Monad (Parser c)

instance parserAlt :: Alt (Parser c) where
    alt :: forall a. Parser c a -> Parser c a -> Parser c a
    alt (Parser run1) (Parser run2) = Parser (\s -> run1 s <|> run2 s)

instance parserPlus :: Plus (Parser c) where
    empty :: forall a. Parser c a
    empty = Parser (\s -> Left "empty")

instance parserAlternative :: Alternative (Parser c)
instance parserMonadZero :: MonadZero (Parser c)

fail :: forall c a. Show c => String -> Parser c a
fail message = Parser (\s -> Left $ message <> ": remaining: " <> View.showView s)

peek :: forall c. Parser c (AnyView c)
peek = Parser (\s -> Right {result: View.eraseView s, remaining: View.eraseView s})

poke :: forall c a. a -> AnyView c -> Parser c a
poke result remaining = Parser (\_ -> Right { result, remaining })

take :: forall c. Show c => Int -> Parser c (AnyView c)
take 0 = pure (View.eraseView View.empty)
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

literal :: forall g v c. Viewable g v c => Eq c => Show c => g -> Parser c g
literal s = do
    let sv = View.view s
    token <- take (View.length sv)
    unless (viewEq token sv) $ fail $ "expected " <> View.showView sv <> "; got " <> show token
    pure s

char :: forall c. Show c => Parser c c
char = do
    c <- take 1
    pure $ unsafePartial $ fromJust $ View.head c <#> (_.head)

digit :: Parser Char Int
digit = do
    c <- char
    unless (c >= '0' && c <= '9') $ fail $ "expected digit; got " <> show c
    pure (Char.toCharCode c - Char.toCharCode '0')

integer ::Parser Char Int
integer = do
    sign <- (literal "-" *> pure (-1)) <|> pure 1
    n <- go 0
    pure (n * sign)
    where
        go i = do
            d <- digit
            let n = i*10 + d
            go n <|> pure n

eof :: forall c. Show c => Parser c Unit
eof = do
    s <- peek
    unless (View.null s) $ fail $ "expected end of string; got " <> show s
    pure unit

eol :: Parser Char Unit
eol = do
    c <- char
    unless (c == '\n') $ fail $ "expected end of line; got '" <> show c <> "'"
    pure unit

zeroOrMore :: forall c a. Parser c a -> Parser c (Array a)
zeroOrMore p = go []
    where
    go accum = (do
        c <- p
        go (Array.snoc accum c)
    ) <|> pure accum

spaces :: Parser Char (Array Char)
spaces = zeroOrMore do
    c <- char
    guard $ c == ' '
    pure c
