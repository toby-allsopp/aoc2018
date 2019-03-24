module Test.Main where

import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.Maybe (Maybe(..))
import Day13 (Cart(..), Direction(..), Turn(..), compareCarts, findCollision, firstCrash, lastRemainingCart, lowerBoundBy, modifyAtSortedBy, moveCart, parseInput, rotate, swap, tick)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Prelude (Ordering(..), Unit, bind, compare, discard, flip, mod, negate, pure, show, ($), (&&), (<>), (==), (>>=))
import Test.QuickCheck (Result(..), (<?>))
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)

main :: Effect Unit
main = runTest do
  test "parseInput" do
    let inputRect = parseInput """/----\
|    |
|    |
\----/"""
    Assert.equal [['/','-','-','-','-','\\'],
                  ['|',' ',' ',' ',' ','|'],
                  ['|',' ',' ',' ',' ','|'],
                  ['\\','-','-','-','-','/']] inputRect.map
    Assert.equal { carts: [] } inputRect.state
    let inputLine = parseInput """|
v
|
|
|
^
|"""
    Assert.equal [['|'],['|'],['|'],['|'],['|'],['|'],['|']] inputLine.map
    Assert.equal { carts: [Cart { x: 0, y: 1, direction: DirDown, nextIntersectionChoice: TurnLeft },
                           Cart { x: 0, y: 5, direction: DirUp, nextIntersectionChoice: TurnLeft }] } inputLine.state
  test "swap" do
    Assert.equal [1] $ unsafePartial $ ST.run (STArray.withArray (swap 0 0) [1])
    Assert.equal [1, 2] $ unsafePartial $ ST.run (STArray.withArray (swap 0 0) [1, 2])
    Assert.equal [2, 1] $ unsafePartial $ ST.run (STArray.withArray (swap 0 1) [1, 2])
  test "rotate" do
    Assert.equal [1] $ unsafePartial $ ST.run (STArray.withArray (rotate 0 0) [1])
    Assert.equal [1, 2] $ unsafePartial $ ST.run (STArray.withArray (rotate 0 0) [1, 2])
    Assert.equal [2, 1] $ unsafePartial $ ST.run (STArray.withArray (rotate 0 1) [1, 2])
    Assert.equal [2, 1] $ unsafePartial $ ST.run (STArray.withArray (rotate 1 0) [1, 2])
    Assert.equal [1, 4, 2, 3, 5] $ unsafePartial $ ST.run (STArray.withArray (rotate 1 3) [1, 2, 3, 4, 5])
    Assert.equal [1, 3, 4, 2, 5] $ unsafePartial $ ST.run (STArray.withArray (rotate 3 1) [1, 2, 3, 4, 5])
  test "lowerBoundBy" do
    Assert.equal 0 $ lowerBoundBy compare 0 []
    Assert.equal 0 $ lowerBoundBy compare 0 [0]
    Assert.equal 1 $ lowerBoundBy compare 1 [0]
    Assert.equal 2 $ lowerBoundBy compare 2 [0, 1, 3, 4]
    Assert.equal 0 $ lowerBoundBy compare (-1) [0, 1, 3, 4]
    Assert.equal 3 $ lowerBoundBy compare 4 [0, 1, 3, 4]
    Assert.equal 4 $ lowerBoundBy compare 5 [0, 1, 3, 4]
    Assert.assert "should be correct index" $ flip elem [2,3,4] $ lowerBoundBy compare 2 [0, 1, 2, 2, 3, 4]
  test "modifyAtSortedBy" do
    let go i x a = unsafePartial $ ST.run do
          a' <- STArray.thaw a
          i' <- modifyAtSortedBy compare i x a'
          a'' <- STArray.freeze a'
          pure { i: i', a: a'' }
    Assert.equal { i: 0, a: [0] } $ go 0 0 [1]
    Assert.equal { i: 0, a: [1] } $ go 0 1 [1]
    Assert.equal { i: 0, a: [2] } $ go 0 2 [1]
    Assert.equal { i: 0, a: [2, 2] } $ go 0 2 [1, 2]
    Assert.equal { i: 1, a: [2, 3] } $ go 0 3 [1, 2]
    Assert.equal { i: 0, a: [0, 1] } $ go 1 0 [1, 2]
    Assert.equal { i: 0, a: [2, 3] } $ go 0 2 [1, 3]
    quickCheck $ \i x a ->
      if Array.null a then Success else
      let index = (i `mod` Array.length a) in
      let array = Array.sort a in
      let { i: i', a: a' } = go index x array in
      Array.sort a' == a' && Array.index a' i' == Just x
        <?> "failed for input " <> show index <> " " <> show x <> " " <> show array <> " -> " <> show i' <> " " <> show a'
  test "compareCarts" do
    Assert.equal' "eq x, eq y" EQ $ compareCarts (Cart { x: 5, y: 7, direction: DirLeft, nextIntersectionChoice: TurnLeft })
                                                 (Cart { x: 5, y: 7, direction: DirRight, nextIntersectionChoice: TurnRight })
    Assert.equal' "lt x, eq y" LT $ compareCarts (Cart { x: 4, y: 7, direction: DirLeft, nextIntersectionChoice: TurnLeft })
                                                 (Cart { x: 5, y: 7, direction: DirRight, nextIntersectionChoice: TurnRight })
    Assert.equal' "gt x, lt y" LT $ compareCarts (Cart { x: 6, y: 6, direction: DirLeft, nextIntersectionChoice: TurnLeft })
                                                 (Cart { x: 5, y: 7, direction: DirRight, nextIntersectionChoice: TurnRight })
    Assert.equal' "gt x, eq y" GT $ compareCarts (Cart { x: 5, y: 7, direction: DirRight, nextIntersectionChoice: TurnRight })
                                                 (Cart { x: 4, y: 7, direction: DirLeft, nextIntersectionChoice: TurnLeft })
    Assert.equal' "lt x, gt y" GT $ compareCarts (Cart { x: 5, y: 7, direction: DirRight, nextIntersectionChoice: TurnRight })
                                                 (Cart { x: 6, y: 6, direction: DirLeft, nextIntersectionChoice: TurnLeft })
  test "moveCart" do
    Assert.equal (Cart { x: 1, y: 0, direction: DirRight, nextIntersectionChoice: TurnLeft }) $ moveCart [['-', '-', '-']] (Cart { x: 0, y: 0, direction: DirRight, nextIntersectionChoice: TurnLeft })
    Assert.equal (Cart { x: 1, y: 0, direction: DirUp, nextIntersectionChoice: Straight }) $ moveCart [['-', '+', '-']] (Cart { x: 0, y: 0, direction: DirRight, nextIntersectionChoice: TurnLeft })
  test "findCollision" do
    Assert.equal Nothing $ findCollision { carts: [] }
    Assert.equal Nothing $ findCollision { carts: [Cart { x: 0, y: 2, direction: DirDown, nextIntersectionChoice: TurnLeft },
                                                   Cart { x: 0, y: 4, direction: DirUp, nextIntersectionChoice: TurnLeft }] }
    Assert.equal (Just {x: 0, y: 3}) $ findCollision { carts: [Cart { x: 0, y: 3, direction: Crashed, nextIntersectionChoice: TurnLeft },
                                                               Cart { x: 0, y: 3, direction: Crashed, nextIntersectionChoice: TurnLeft }] }
  test "tick" do
    let input = parseInput """|
v
|
|
|
^
|"""
    Assert.equal (Right { carts: [Cart { x: 0, y: 2, direction: DirDown, nextIntersectionChoice: TurnLeft },
                                  Cart { x: 0, y: 4, direction: DirUp, nextIntersectionChoice: TurnLeft }] }) $ tick input.map findCollision input.state
    Assert.equal (Left { x: 0, y: 3 }) $ tick input.map findCollision input.state >>= tick input.map findCollision
  test "part 1" do
    let input = parseInput """/->-\        
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   """
    Assert.equal { x: 7, y: 3 } $ firstCrash input.map input.state
  test "part 2" do
    let input = parseInput """/>-<\  
|   |  
| /<+-\
| | | v
\>+</ |
  |   ^
  \<->/"""
    Assert.equal { x: 6, y: 4 } $ (\(Cart { x, y }) -> { x, y }) $ lastRemainingCart input.map input.state