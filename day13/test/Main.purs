module Test.Main where

import Day13
import Prelude

import Control.Monad.ST as ST
import Data.Array.ST as STArray
import Data.Either (Either(..))
import Data.Foldable (elem)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

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
          e <- modifyAtSortedBy compare i x a'
          a'' <- STArray.freeze a'
          pure { e, a: a'' }
    Assert.equal { e: 1, a: [0] } $ go 0 0 [1]
    Assert.equal { e: 1, a: [1] } $ go 0 1 [1]
    Assert.equal { e: 1, a: [2] } $ go 0 2 [1]
    Assert.equal { e: 2, a: [2, 2] } $ go 0 2 [1, 2]
    Assert.equal { e: 2, a: [2, 3] } $ go 0 3 [1, 2]
    Assert.equal { e: 1, a: [0, 1] } $ go 1 0 [1, 2]
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
  test "tick" do
    let input = parseInput """|
v
|
|
|
^
|"""
    Assert.equal (Right { carts: [Cart { x: 0, y: 2, direction: DirDown, nextIntersectionChoice: TurnLeft },
                                  Cart { x: 0, y: 4, direction: DirUp, nextIntersectionChoice: TurnLeft }] }) $ tick input.map input.state
    Assert.equal (Left { x: 0, y: 3 }) $ tick input.map input.state >>= tick input.map
  test "part 1" do
    let input = parseInput """/->-\        
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   """
    Assert.equal { x: 7, y: 3 } $ firstCrash input.map input.state
