module Parser where

import Control.MonadZero (class Alt, class Alternative, class MonadZero, class Plus, unless, (<|>))
import Data.Char as Char
import Data.Either (Either(..))
import Data.String as String
import Data.String.Unsafe as String.Unsafe
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, Unit, ap, bind, discard, pure, show, unit, (#), ($), (&&), (*), (+), (-), (<#>), (<=), (<>), (==), (>=), (>=>))
  
data Parser a = Parser (String -> Either String { result:: a, remaining:: String })

runParser :: forall a. Parser a -> String -> Either String a
runParser (Parser run) s = run s <#> (_.result)

instance parserFunctor :: Functor Parser where
    map :: forall a b. (a -> b) -> Parser a -> Parser b
    map f = \(Parser run) -> Parser (\s -> run s <#> (\{result, remaining} -> {result: f result, remaining}))

instance parserApply :: Apply Parser where
    apply :: forall a b. Parser (a -> b) -> Parser a -> Parser b
    apply = ap

instance parserApplicative :: Applicative Parser where
    pure :: forall a. a -> Parser a
    pure x = Parser (\s -> Right {result: x, remaining: s})

instance parserBind :: Bind Parser where
    bind :: forall a b. Parser a -> (a -> Parser b) -> Parser b
    bind (Parser run) f = Parser (run >=> (\{result, remaining} -> f result # (\(Parser runF) -> runF remaining)))

instance parserMonad :: Monad Parser

instance parserAlt :: Alt Parser where
    alt :: forall a. Parser a -> Parser a -> Parser a
    alt (Parser run1) (Parser run2) = Parser (\s -> run1 s <|> run2 s)

instance parserPlus :: Plus Parser where
    empty :: forall a. Parser a
    empty = Parser (\s -> Left "empty")

instance parserAlternative :: Alternative Parser
instance parserMonadZero :: MonadZero Parser

fail :: forall a . String -> Parser a
fail message = Parser (\s -> Left $ message <> ": remaining: " <> s)

peek :: Parser String
peek = Parser (\s -> Right {result: s, remaining: s})

poke :: forall a. a -> String -> Parser a
poke result remaining = Parser (\_ -> Right {result, remaining})

take :: Int -> Parser String
take n = do
    s <- peek
    let {before, after} = String.splitAt n s
    unless (String.length before == n) $ fail $ "expected " <> show n <> " chars; got " <> show (String.length before)
    poke before after

literal :: String -> Parser String
literal s = do
    token <- take (String.length s)
    unless (token == s) $ fail $ "expected " <> s <> "; got " <> token
    pure token

char :: Parser Char
char = do
    c <- take 1
    pure (String.Unsafe.char c)

digit :: Parser Int
digit = do
    c <- char
    unless (c >= '0' && c <= '9') $ fail $ "expected digit; got " <> show c
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
    unless (String.null s) $ fail $ "expected end of string; got " <> s
    pure unit
