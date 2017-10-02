module Main where

import           Control.Exception (evaluate)
import           Data.List         (permutations, sort)
import           Prelude           hiding (fail)
import           System.Random     (newStdGen, randomRs)
import           Test.Hspec

import           Lib               (collectEvery, contains, highestBit, joinWith,
                                    mergeSort, order3, removeAt, smartReplicate, splitOn,
                                    stringSum)


main :: IO ()
main = hspec $ do
    describe "order3" $
        it "works on all permutations [1, 2, 3]" $
            let perms    = permutations [1 :: Integer, 2, 3]
                result   = map (order3 . toTuple) perms
                expected = map (toTuple . sort) perms
                toTuple [a, b, c] = (a, b, c)
                toTuple _         = error "unreachable"
            in result `shouldBe` expected

    describe "highestBit" $ do
        it "works on pretests" $
            let testData = [15 :: Integer, 16, 17]
                expected = [(8, 3), (16, 4), (16, 4)]
                result   = map highestBit testData
            in result `shouldBe` expected
        it "works on negative input" $
            highestBit (-100500 :: Integer) `shouldBe` (0 :: Integer, -1 :: Integer)

    describe "smartReplicate" $ do
        it "works on pretest" $
            smartReplicate [1 :: Int, 2, 3] `shouldBe` [1, 2, 2, 3, 3, 3]
        it "works on zero" $
            smartReplicate [0 :: Int] `shouldBe` []
        it "works on empty list" $
            smartReplicate [] `shouldBe` []

    describe "contains" $ do
        it "works on pretest" $
            contains (3 :: Integer) [[1 .. 5], [2, 0], [3, 4]] `shouldBe` [[1 .. 5], [3, 4]]
        it "works on empty lists" $
            contains (3 :: Integer) [[], []] `shouldBe` []
        it "works on empty list" $
            contains (3 :: Integer) [] `shouldBe` []

    describe "removeAt" $ do
        it "works on pretests" $
            let testData = [[1 :: Integer, 2, 3], [1, 2, 3]           , [1 .. 5]              ]
                testIdx  = [1                   , 10                  , 3                     ]
                expected = [(Just 2, [1, 3])    , (Nothing, [1, 2, 3]), (Just 4, [1, 2, 3, 5])]
            in zipWith removeAt testIdx testData `shouldBe` expected
        it "works on pretests..part 2" $
            removeAt 2 "abc" `shouldBe` (Just 'c', "ab")
        it "fails on negative index" $
            evaluate (removeAt (-100500) "ahahaha") `shouldThrow` errorCall "Negative index"

    describe "collectEvery" $
        it "works on pretests" $
            collectEvery 3 [(1 :: Integer) .. 8] `shouldBe` ([1, 2, 4, 5, 7, 8], [3, 6])

    describe "stringSum" $ do
        it "works on basic" $
            let testData = [ "1", "1 2 3", " 1", "1 ", "\t1\t", "\t12345\t", "010 020 030"
                           , " 123 456 789 ", "-1", "-1 -2 -3", "\t-12345\t", " -123 -456 -789 "
                           , "\n1\t\n3   555  -1\n\n\n-5", "123\t\n\t\n\t\n321 -4 -40"
                           ]
                expected = [1 :: Int, 6, 1, 1, 1, 12345, 60
                           , 123 + 456 + 789, -1, -6, -12345, -123 - 456 - 789
                           , 1 + 3 + 555 - 1 - 5, 123 + 321 - 4 - 40
                           ]
            in map stringSum testData `shouldBe` expected
        it "fails on basic" $
            let testData = ["asd", "1-1", "1.2", "--2", "1+"]
            in mapM_ (\input -> (evaluate . stringSum) input `shouldThrow` anyErrorCall) testData
        it "works on advaned" $
            let testData = ["+1", "1 +1", "-1 +1", "+1 -1"]
                expected = [1 :: Int, 2, 0, 0]
            in map stringSum testData `shouldBe` expected
        it "fails on advanced" $
            let testData = ["1+1", "++1", "-+1", "+-1", "1 + 1"]
            in mapM_ (\input -> (evaluate . stringSum) input `shouldThrow` anyErrorCall) testData

    describe "mergeSort" $ do
        it "works on empty list" $
            mergeSort ([] :: [Int]) `shouldBe` []
        it "works on small lists" $ do
            testData <- mapM (\len -> randomIntList len (-100) 100) [1 .. 100]
            mapM_ (\list -> mergeSort list `shouldBe` Data.List.sort list) testData
        it "works on large lists" $ do
            testData <- mapM (\len -> randomIntList len (-100500) 100500) [12345, 2 ^ (16::Int), 3 ^ (10 :: Int)]
            mapM_ (\list -> mergeSort list `shouldBe` Data.List.sort list) testData

    describe "splitOn" $ do
        it "works on pretest" $
            splitOn '/' "path/to/file" `shouldBe` ["path", "to", "file"]
        it "works on empty string" $
            splitOn '/' "" `shouldBe` [""]
        it "works on string without separator" $
            splitOn '/' "there is no slash here!" `shouldBe` ["there is no slash here!"]

    describe "joinWith" $ do
        it "works on pretest" $
            joinWith '/' ["path", "to", "file"] `shouldBe` "path/to/file"
        it "works on empty list" $
            joinWith '/' ([] :: [String]) `shouldBe` ""
        it "works on single string" $
            joinWith '/' ["i am single"] `shouldBe` "i am single"

    describe "joinWith . splitOn" $
        it "work together" $
            let testData = [ "path/to/file"
                           , "path/to/dir/"
                           , "/path/from/root/to/file"
                           , "/path/from/root/to/dir"
                           ]
            in mapM_ (\text -> joinWith '/' (splitOn '/' text) `shouldBe` text) testData


randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen
