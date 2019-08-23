module Parser where

import Prelude

import Control.Lazy (class Lazy)
import Control.Monad.Rec.Class (Step(..), class MonadRec, tailRecM)
import Control.MonadZero (class Alt, class Alternative, class MonadZero, class Plus, guard, (<|>))
import Data.Array as Array
import Data.Char as Char
import Data.Char.Unicode (isSpace)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String.CodeUnits as String
import Partial.Unsafe (unsafePartial)

class Parseable f c | f -> c where
    pempty :: f
    pnull :: f -> Boolean
    plength :: f -> Int
    psplitAt :: Int -> f -> Maybe { before :: f, after :: f }
    phead :: f -> Maybe { head :: c, tail :: f }
    pcons :: c -> f -> f

ptake :: forall s c. Parseable s c => Int -> s -> s
ptake n s = psplitAt n s <#> _.before # fromMaybe s

ptakeWhile :: forall s c. Parseable s c => (c -> Boolean) -> s -> { before :: s, after :: s }
ptakeWhile f s =
    case phead s of
        Just { head, tail } ->
            if f head then
                let { before, after } = ptakeWhile f tail in
                { before: pcons head before, after }
            else
                { before: pempty, after: s }
        Nothing -> { before: pempty, after: s }

instance parseableString :: Parseable String Char where
    pempty  = ""
    pnull = String.length >>> (_ == 0)
    plength = String.length
    psplitAt n s =
        if String.length s >= n then
            Just { before: String.take n s, after: String.drop n s }
        else
            Nothing
    phead s =
        case String.charAt 0 s of
            Just head -> Just { head, tail: String.drop 1 s }
            Nothing -> Nothing
    pcons c s = (String.singleton c) <> s

instance parseableArray :: Parseable (Array c) c where
    pempty = []
    pnull = Array.length >>> (_ == 0)
    plength = Array.length
    psplitAt n a =
        if Array.length a >= n then
            Just { before: Array.take n a, after: Array.drop n a }
        else
            Nothing
    phead a = Array.head a <#> { head: _, tail: Array.drop 1 a }
    pcons = Array.cons

data Parser f a = Parser (f -> Either String { result:: a, remaining:: f })

runParser :: forall f c a. Parseable f c => Parser f a -> f -> Either String a
runParser (Parser run) s = run s <#> (_.result)

instance parserFunctor :: Functor (Parser c) where
    map :: forall a b. (a -> b) -> Parser c a -> Parser c b
    map f = \(Parser run) -> Parser (\s -> run s <#> (\{result, remaining} -> {result: f result, remaining}))

instance parserApply :: Apply (Parser c) where
    apply :: forall a b. Parser c (a -> b) -> Parser c a -> Parser c b
    apply = ap

instance parserApplicative :: Applicative (Parser c) where
    pure :: forall a. a -> Parser c a
    pure x = Parser (\s -> Right {result: x, remaining: s})

instance parserBind :: Bind (Parser c) where
    bind :: forall a b. Parser c a -> (a -> Parser c b) -> Parser c b
    bind (Parser run) f = Parser (run >=> (\{result, remaining} -> f result # (\(Parser runF) -> runF remaining)))

instance parserMonad :: Monad (Parser c)

instance parserAlt :: Alt (Parser c) where
    alt :: forall a. Parser c a -> Parser c a -> Parser c a
    alt (Parser run1) (Parser run2) = Parser (\s -> run1 s <|> run2 s)

instance parserPlus :: Plus (Parser c) where
    empty :: forall a. Parser c a
    empty = Parser (\s -> Left "empty")

instance parserAlternative :: Alternative (Parser c)
instance parserMonadZero :: MonadZero (Parser c)

instance parserMonadRec :: Parseable s c => MonadRec (Parser s) where
    tailRecM :: forall a b. (a -> Parser s (Step a b)) -> a -> Parser s b
    tailRecM f a0 = Parser (tailRecM' f a0)

tailRecM' :: forall a b c v. Parseable v c =>  (a -> Parser v (Step a b)) -> a -> v -> Either String { result :: b, remaining :: v }
tailRecM' f a s =
    case f a of Parser run ->
        case run s of
            Left err -> Left err
            Right {result: Loop next, remaining} -> tailRecM' f next remaining
            Right {result: Done result, remaining} -> Right { result, remaining }

instance parserLazy :: Lazy (Parser c a) where
    defer :: (Unit -> Parser c a) -> Parser c a
    defer f = Parser \s -> let Parser run = f unit in run s

fail :: forall s c a. Parseable s c => Show s => String -> Parser s a
fail message = Parser (\s -> Left $ message <> ": remaining: " <> show (ptake 80 s))

peek :: forall s c. Parseable s c => Parser s s
peek = Parser (\s -> Right {result: s, remaining: s})

poke :: forall c a. a -> c -> Parser c a
poke result remaining = Parser (\_ -> Right { result, remaining })

take :: forall s c. Parseable s c => Show s => Int -> Parser s s
take 0 = pure pempty
take n = do
    s <- peek
    case psplitAt n s of
        Just {before, after} -> poke before after
        Nothing -> fail $ "expected " <> show n <> " chars; got " <> show (plength s)

takeWhile :: forall s c. Parseable s c => Show c => (c -> Boolean) -> Parser s s
takeWhile p = do
    s <- peek
    case ptakeWhile p s of
        {before, after} -> poke before after

literal :: forall s c. Parseable s c => Eq s => Show s => s -> Parser s s
literal s = do
    token <- take (plength s)
    unless (token == s) $ fail $ "expected " <> show s <> "; got " <> show token
    pure s

char :: forall s c. Parseable s c => Show s => Parser s c
char = do
    c <- take 1
    pure $ unsafePartial $ fromJust $ phead c <#> (_.head)

word :: Parser String String
word = takeWhile (not <<< isSpace)

digit :: Parser String Int
digit = do
    c <- char
    unless (c >= '0' && c <= '9') $ fail $ "expected digit; got " <> show c
    pure (Char.toCharCode c - Char.toCharCode '0')

integer ::Parser String Int
integer = do
    sign <- (literal "-" *> pure (-1)) <|> pure 1
    n <- go 0
    pure (n * sign)
    where
        go i = do
            d <- digit
            let n = i*10 + d
            go n <|> pure n

eof :: forall s c. Parseable s c => Show s => Parser s Unit
eof = do
    s <- peek
    unless (pnull s) $ fail $ "expected end of string; got " <> show s
    pure unit

eol :: Parser String Unit
eol = do
    c <- char
    unless (c == '\n') $ fail $ "expected end of line; got '" <> show c <> "'"
    pure unit

zeroOrMore :: forall s c a. Parseable s c => Parser s a -> Parser s (Array a)
zeroOrMore p = tailRecM go []
    where
    go accum = (do
        c <- p
        pure $ Loop (Array.snoc accum c)
    ) <|> pure (Done accum)

spaces :: Parser String (Array Char)
spaces = zeroOrMore do
    c <- char
    guard $ c == ' '
    pure c

separatedList :: forall s c a b. Parseable s c => Parser s b -> Parser s a -> Parser s (Array a)
separatedList sepParser elementParser = tailRecM go []
    where
    go accum = (do
        head <- elementParser
        (do
            _ <- sepParser
            pure $ Loop (Array.snoc accum head)
        ) <|> pure (Done (Array.snoc accum head))
    ) <|> pure (Done accum)
