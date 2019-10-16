module Test.Main where

import Prelude
import Data.Maybe (Maybe(..), fromJust)
import Day23 as Day23
import Day23 (Pos(..))
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main =
  runTest do
    suite "bounds" do
      test "none" do
        Assert.equal Nothing $ Day23.bounds []
      test "one" do
        Assert.equal (Just { minBound: (Pos 0 0 0), maxBound: (Pos 0 0 0) })
          $ Day23.bounds [ { pos: (Pos 0 0 0), r: 1 } ]
      test "two" do
        Assert.equal (Just { minBound: (Pos 0 0 0), maxBound: Pos 10 10 10 })
          $ Day23.bounds
              [ { pos: Pos 10 0 10, r: 1 }
              , { pos: Pos 0 10 0, r: 1 }
              ]
    suite "distance" do
      test "tes1" do
        Assert.equal 2 $ Day23.distance (Pos 0 0 0) (Pos 0 1 1)
    suite "overlaps" do
      test "contained within box" do
        Assert.equal true $ Day23.overlaps (Pos (-10) (-10) (-10)) (Pos 10 10 10) { pos: (Pos 0 0 0), r: 2 }
      test "outside box" do
        Assert.equal false $ Day23.overlaps (Pos (-10) (-10) (-10)) (Pos 10 10 10) { pos: (Pos 20 20 20), r: 2 }
      test "overlaps box" do
        Assert.equal true $ Day23.overlaps (Pos (-10) (-10) (-10)) (Pos 10 10 10) { pos: (Pos 12 10 10), r: 2 }
      test "something" do
        Assert.equal false $ Day23.overlaps (Pos 0 1 1) (Pos 0 1 1) { pos: Pos 0 0 0, r: 1 }
    suite "divide" do
      test "single point" do
        let
          bots = [ { pos: (Pos 0 0 0), r: 1 } ]
        let
          { minBound, maxBound } = unsafePartial $ fromJust $ Day23.bounds bots
        Assert.equal [ { bots, minBound, maxBound } ] $ Day23.divide { bots, minBound, maxBound }
      test "two points" do
        let
          b1 = { pos: (Pos 10 10 10), r: 1 }

          b2 = { pos: (Pos 11 11 11), r: 1 }
        let
          bots = [ b1, b2 ]
        let
          { minBound, maxBound } = unsafePartial $ fromJust $ Day23.bounds bots
        Assert.equal
          [ { bots: [ b1 ], minBound: (Pos 10 10 10), maxBound: (Pos 10 10 10) }
          , { bots: [ b1 ], minBound: (Pos 10 10 11), maxBound: (Pos 10 10 11) }
          , { bots: [ b1 ], minBound: (Pos 10 11 10), maxBound: (Pos 10 11 10) }
          , { bots: [ b2 ], minBound: (Pos 10 11 11), maxBound: (Pos 10 11 11) }
          , { bots: [ b1 ], minBound: (Pos 11 10 10), maxBound: (Pos 11 10 10) }
          , { bots: [ b2 ], minBound: (Pos 11 10 11), maxBound: (Pos 11 10 11) }
          , { bots: [ b2 ], minBound: (Pos 11 11 10), maxBound: (Pos 11 11 10) }
          , { bots: [ b2 ], minBound: (Pos 11 11 11), maxBound: (Pos 11 11 11) }
          ]
          $ Day23.divide { bots, minBound, maxBound }
      test "three points" do
        let
          b1 = { pos: (Pos 0 0 0), r: 1 }

          b2 = { pos: (Pos 1 1 1), r: 1 }

          b3 = { pos: (Pos 10 10 10), r: 1 }
        let
          bots = [ b1, b2, b3 ]
        let
          { minBound, maxBound } = unsafePartial $ fromJust $ Day23.bounds bots
        Assert.equal
          [ { bots: [ b1, b2 ], minBound: (Pos 0 0 0), maxBound: (Pos 4 4 4) }
          , { bots: [], minBound: (Pos 0 0 5), maxBound: (Pos 4 4 10) }
          , { bots: [], minBound: (Pos 0 5 0), maxBound: (Pos 4 10 4) }
          , { bots: [], minBound: (Pos 0 5 5), maxBound: (Pos 4 10 10) }
          , { bots: [], minBound: (Pos 5 0 0), maxBound: (Pos 10 4 4) }
          , { bots: [], minBound: (Pos 5 0 5), maxBound: (Pos 10 4 10) }
          , { bots: [], minBound: (Pos 5 5 0), maxBound: (Pos 10 10 4) }
          , { bots: [ b3 ], minBound: (Pos 5 5 5), maxBound: (Pos 10 10 10) }
          ]
          $ Day23.divide { bots, minBound, maxBound }
