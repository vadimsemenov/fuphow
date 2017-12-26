{-# LANGUAGE TemplateHaskell #-}

module TemplateLibSpec
       ( spec
       ) where

import           Test.Hspec

import qualified Data.Text as T
import           TemplateLib (chooseByIndices, showText, deriveOverloadedShow)

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
    describe "Derive OverloadedShow" $ do
        it "Unit" $ do
            showText Unit `shouldBe` T.pack "Unit"
        it "MyData" $
            showText MyData { intRecord = 100500, stringRecord = "hi" } `shouldBe`
                T.pack "MyData {intRecord = 100500, stringRecord = \"hi\"}"


data Unit = Unit
    deriving (Show)

data MyData = MyData { intRecord :: Int
                     , stringRecord :: String
                     }
    deriving (Show)

deriveOverloadedShow ''Unit
deriveOverloadedShow ''MyData
