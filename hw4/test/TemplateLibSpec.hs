{-# LANGUAGE TemplateHaskell #-}

module TemplateLibSpec
       ( spec
       ) where

import           Test.Hspec

import           TemplateLib (chooseByIndices)

spec :: Spec
spec = do
    describe "Choose by indices" $ do
        let sampleTuple = ("hello", 10, [4,3], 2) :: (String, Int, [Int], Int)
        it "first sample" $
            $(chooseByIndices 4 [2, 0]) sampleTuple `shouldBe`
                (([4, 3], "hello") :: ([Int], String))
        it "second sample" $
            $(chooseByIndices 4 [1, 1, 3, 1, 1]) sampleTuple `shouldBe`
                ((10, 10, 2, 10, 10) :: (Int, Int, Int, Int, Int))
