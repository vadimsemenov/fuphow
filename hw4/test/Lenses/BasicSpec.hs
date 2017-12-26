module Lenses.BasicSpec
       ( spec
       ) where

import           Lenses.Basic
import           Test.Hspec

spec :: Spec
spec = describe "Basic lenses with (String, Int)" $ do
    let hello = "hello"
    let world = "world"
    let one = 1 :: Int
    let two = 2 :: Int
    let pair = (hello, one)
    context "set" $ do
        it "_1" $
            set _1 world pair `shouldBe` (world, one)
        it "_2" $
            set _2 two pair `shouldBe` (hello, two)
    context "view" $ do
        it "_1" $
            view _1 pair `shouldBe` hello
        it "_2" $
            view _2 pair `shouldBe` one
    context "over" $ do
        it "_1 (const world)" $
            over _1 (const world) pair `shouldBe` (world, one)
        it "_2 (+1)" $
            over _2 (+1) pair `shouldBe` (hello, two)
