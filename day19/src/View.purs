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

showView :: forall v a. View v a => Show a => Int -> v -> String
showView maxLength v =
    if maxLength > 0 then
        case head v of
        Just { head, tail } -> "(View " <> show head <> " " <> showView (maxLength - 1) tail <> ")"
        Nothing -> "(View)"
    else
        "(View ...)"

null :: forall v a. View v a => v -> Boolean
null v = length v == 0

toUnfoldable :: forall v a f. View v a => Unfoldable f => v -> f a
toUnfoldable = unfoldr go
    where
        go :: v -> Maybe (Tuple a v)
        go v = head v <#> (\h -> Tuple h.head h.tail)

data EmptyView a = EmptyView

empty :: forall a. EmptyView a
empty = EmptyView

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

allIndexed :: forall f a. Indexable f a => f -> IndexableView f a
allIndexed xs = IndexableView { indexable : xs, begin : 0, end : lengthIndexable xs }

takeWhile :: forall v a. View v a => (a -> Boolean) -> v -> { before :: Array a, after :: v }
takeWhile p v =
    case head v of
        Just { head, tail } ->
            if p head then
                let { before, after } = takeWhile p tail in
                { before: Array.cons head before, after }
            else
                { before: [], after: v }
        Nothing -> { before: [], after: v }

data AnyView c = AnyView (forall a. (forall v. View v c => v -> a) -> a)

eraseView :: forall v c. View v c => v -> AnyView c
eraseView v = AnyView (\f -> f v)

uneraseView :: forall a c. (forall v. View v c => v -> a) -> AnyView c -> a
uneraseView f (AnyView run) = run f

instance anyViewShow :: Show a => Show (AnyView a) where
    show = uneraseView (showView 80)

instance emptyViewView :: View (EmptyView a) a where
    head _ = Nothing
    splitAt _ _ = Nothing
    length _ = 0

else instance indexableViewView :: Indexable f a => View (IndexableView f a) a where
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

else instance anyViewView :: View (AnyView c) c where
    head = uneraseView (\v -> head v <#> \cons -> { head : cons.head, tail : eraseView cons.tail })
    splitAt n = uneraseView (\v -> splitAt n v <#> \{ before, after } -> { before : eraseView before, after : eraseView after })
    length = uneraseView length

class View v a <= Viewable f v a | f -> v, f -> a where
    view :: f -> v

instance indexableViewable :: Indexable f a => Viewable f (IndexableView f a) a where
    view = allIndexed

else instance viewViewable :: View v a => Viewable v v a where
    view = identity

testView :: forall v a. View v a => v -> Array a
testView v = case head v of
    Just { head, tail } -> Array.cons head (testView tail)
    Nothing -> []

testArray = testView $ allIndexed [1, 2, 3]

testString = testView $ allIndexed "123"