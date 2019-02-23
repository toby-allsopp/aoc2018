module View where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)

class View v a | v -> a where
    head :: v -> Maybe { head :: a, tail :: v }
    splitAt :: Int -> v -> Maybe { before :: v, after :: v }
    length :: v -> Int
    empty :: v

null :: forall v a. View v a => v -> Boolean
null v = length v == 0

toUnfoldable :: forall v a f. View v a => Unfoldable f => v -> f a
toUnfoldable = unfoldr go
    where
        go :: v -> Maybe (Tuple a v)
        go v = head v <#> (\h -> Tuple h.head h.tail)

class Indexable f a | f -> a where
    index :: f -> Int -> Maybe a
    lengthIndexable :: f -> Int
    emptyIndexable :: f

instance arrayIndexable :: Indexable (Array a) a where
    index = Array.index
    lengthIndexable = Array.length
    emptyIndexable = []

instance stringIndexable :: Indexable String Char where
    index = flip String.charAt
    lengthIndexable = String.length
    emptyIndexable = ""

data IndexableView f a = IndexableView { indexable :: f, begin :: Int, end :: Int }

instance indexableViewShow :: Show f => Show (IndexableView f a) where
    show (IndexableView r) = "IndexableView " <> show r

instance indexableViewView :: Indexable f a => View (IndexableView f a) a where
    head (IndexableView { indexable, begin, end }) =
        if begin < end then
            index indexable begin <#> { head : _, tail : IndexableView { indexable, begin : begin + 1, end } }
        else
            Nothing

    splitAt n (IndexableView { indexable, begin, end }) =
        if n <= end - begin then
            Just {
                before : IndexableView { indexable, begin, end : begin + n },
                after  : IndexableView { indexable, begin : begin + n, end }
            }
        else
            Nothing

    length (IndexableView { indexable, begin, end }) = end - begin

    empty = IndexableView { indexable : emptyIndexable, begin : 0, end : 0 }

allIndexed :: forall f a. Indexable f a => f -> IndexableView f a
allIndexed xs = IndexableView { indexable : xs, begin : 0, end : lengthIndexable xs }

testView :: forall v a. View v a => v -> Array a
testView v = case head v of
    Just { head, tail } -> Array.cons head (testView tail)
    Nothing -> []

testArray = testView $ allIndexed [1, 2, 3]

testString = testView $ allIndexed "123"