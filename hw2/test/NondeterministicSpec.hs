module NondeterministicSpec
       ( spec
       ) where

import Test.Hspec

import Nondeterministic (bin, combinations, permutations)
import qualified Data.List as L (sort, reverse, permutations)


spec :: Spec
spec = do
    describe "bin" $ do
        it "works on 0" $
            bin 0 `shouldBe` [ [] ]
        it "works on 1" $
            bin 1 `shouldBe` [ [0], [1] ]
        it "works on 2" $
            bin 2 `shouldBe` [ [ 0 , 0 ] , [ 0 , 1 ] , [ 1 , 0 ] , [ 1 , 1 ] ]
        it "works on 3" $
            bin 3 `shouldBe`[ [ 0 , 0 , 0 ]
                            , [ 0 , 0 , 1 ]
                            , [ 0 , 1 , 0 ]
                            , [ 0 , 1 , 1 ]
                            , [ 1 , 0 , 0 ]
                            , [ 1 , 0 , 1 ]
                            , [ 1 , 1 , 0 ]
                            , [ 1 , 1 , 1 ]
                            ]

    describe "combinations" $ do
        context "on negative numbers" $ do
            it "works when n = -4, k = -7" $
                combinations (-4) (-7) `shouldBe` [ ]
            it "works when n = 4, k = -7" $
                combinations 4 (-7) `shouldBe` [ ]
            it "works when n = -4, k = 7" $
                combinations (-4) 7 `shouldBe` [ ]
        context "n = 0" $ do
            it "works when k = 0" $
                combinations 0 0 `shouldBe` [ [] ]
            it "works when k = 1" $
                combinations 0 1 `shouldBe` [ ]
            it "works when k = 100500" $
                combinations 0 100500 `shouldBe` [ ]
        context "n = 1" $ do
            it "works when k = 0" $
                combinations 1 0 `shouldBe` [ [] ]
            it "works when k = 1" $
                combinations 1 1 `shouldBe` [ [1] ]
            it "works when k = 100500" $
                combinations 1 100500 `shouldBe` [ ]
        context "n = 4" $ do
            it "works when k = 0" $
                combinations 4 0 `shouldBe` [ [] ]
            it "works when k = 2" $
                combinations 4 2 `shouldBe` [ [1, 2]
                                            , [1, 3]
                                            , [1, 4]
                                            , [2, 3]
                                            , [2, 4]
                                            , [3, 4]
                                            ]
            it "works when k = 100500" $
                combinations 4 100500 `shouldBe` [ ]

    describe "permutations" $ do
        it "works on sample" $
            (L.reverse . L.sort) (permutations [22, 10, 5]) `shouldBe` [ [22, 10, 5]
                                                       , [22, 5, 10]
                                                       , [10, 22, 5]
                                                       , [10, 5, 22]
                                                       , [5, 22, 10]
                                                       , [5, 10, 22]
                                                       ]
