module Test.Main where

import Prelude

import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

import Data.Either (Either(..))
import Data.HashMap as HashMap
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Regex (Dir(..), Regex(..))
import Regex as Regex
import Day20 as Day20

main :: Effect Unit
main = runTest do
  suite "parse regex" do
    test "malformed" do
      Assert.assert "expected Left" $ case Regex.parse "hi" of
        Left _ -> true
        Right _ -> false
    test "empty regex" do
      Assert.equal (Right $ Sequence []) (Regex.parse "^$")
    test "single atom" do
      Assert.equal (Right $ Sequence [Atom N]) (Regex.parse "^N$")
    test "multiple atoms" do
      Assert.equal (Right $ Sequence [Atom N, Atom E, Atom W, Atom S]) (Regex.parse "^NEWS$")
    test "empty choice" do
      Assert.equal (Right $ Sequence [Branch [Sequence []]]) (Regex.parse "^()$")
    test "multichoice" do
      Assert.equal (Right $ Sequence [Branch [Sequence [Atom N], Sequence [Atom E, Atom W], Sequence []]]) (Regex.parse "^(N|EW|)$")
    test "nested branches" do
      Assert.equal
        (Right $ Sequence [Branch [Sequence [Atom N],
                                   Sequence [Atom E, Atom W, Branch [Sequence [Atom E, Atom E],
                                                                     Sequence [Atom N]]]]])
        (Regex.parse "^(N|EW(EE|N))$")
  suite "print regex" do
    test "empty regex" do
      Assert.equal "" (Regex.ppRegex (Sequence []))
    test "single atom" do
      Assert.equal "N" (Regex.ppRegex (Sequence [Atom N]))
    test "empty choice" do
      Assert.equal "()" (Regex.ppRegex $ Sequence [Branch [Sequence []]])
    test "multichoice" do
      Assert.equal "(N|EW|)" (Regex.ppRegex $ Sequence [Branch [Sequence [Atom N], Sequence [Atom E, Atom W], Sequence []]])
  suite "follow" do
    test "empty regex" do
      Assert.equal (HashMap.fromArray [(Tuple { x: 0, y: 0 } { shortestDistance: 0 })]) $
        Day20.followRegex (Sequence [])
    test "single atom" do
      Assert.equal
        (HashMap.fromArray [(Tuple { x: 0, y: 0 } { shortestDistance: 0 }),
                            (Tuple { x: 0, y: 1 } { shortestDistance: 1 })]) $
        Day20.followRegex (Sequence [Atom N])
    test "multiple atoms" do
      Assert.equal
        (HashMap.fromArray [(Tuple { x: 0, y: 0 } { shortestDistance: 0 }),
                            (Tuple { x: 0, y: 1 } { shortestDistance: 1 }),
                            (Tuple { x: 1, y: 1 } { shortestDistance: 2 })]) $
        Day20.followRegex (Sequence [Atom N, Atom E, Atom W, Atom S])
    test "branch" do
      Assert.equal
        (HashMap.fromArray [(Tuple { x: 0,  y: 0  } { shortestDistance: 0 }),
                            (Tuple { x: 0,  y: 1  } { shortestDistance: 1 }),
                            (Tuple { x: 1,  y: 0  } { shortestDistance: 1 }),
                            (Tuple { x: -1, y: 0  } { shortestDistance: 1 }),
                            (Tuple { x: 0,  y: -1 } { shortestDistance: 1 })]) $
        Day20.followRegex (Sequence [Branch [Atom N, Atom E, Atom W, Atom S]])
  suite "furthest" do
    let t expected regex = Assert.equal (Right expected) (Regex.parse regex <#> Day20.followRegex <#> Day20.furthest)
    test "ex1" $ t 3 "^WNE$"
    test "ex2" $ t 10 "^ENWWW(NEEE|SSE(EE|N))$"
    test "ex3" $ t 18 "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
    test "ex4" $ t 23 "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
    test "ex5" $ t 31 "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"
