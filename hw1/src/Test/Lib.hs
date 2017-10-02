module Test.Lib
       ( testAll
       , testOrder3'
       , testHighestBit'
       , testSmartReplicate'
       , testContains'
       , testRemoveAt'
       , testCollectEvery'

       , stringSum
       , stringSumPassTests
       , stringSumMustFail
       , stringSumAdvancedTests
       , stringSumAdvancedMustFail

       , testSplitOn'
       , testJoinWith'
       , testSplitJoin
       , testMergeSort'
       , randomIntList
       ) where

import           Data.List     (permutations, sort)
import           Prelude       hiding (fail)
import           System.Random (newStdGen, randomRs)

import           Lib           (collectEvery, contains, highestBit, joinWith, mergeSort,
                                order3, removeAt, smartReplicate, splitOn, stringSum)


-- order3
testOrder3 :: Bool
testOrder3 = testIt "order3" testOrder3' (permutations [1::Integer, 2, 3])

testOrder3' :: (Ord a) => [a] -> Bool
testOrder3' ls = toTuple (sort ls) == order3 (toTuple ls)
  where
    toTuple [a, b, c] = (a, b, c)
    toTuple _         = undefined

-- highesetBit
testHighestBit :: Bool
testHighestBit = testIt "highestBit" testHighestBit' [ (15::Integer, (8, 3))
                                                     , (16, (16, 4))
                                                     , (17, (16, 4))
                                                     , (0, (0, -1))
                                                     ]

testHighestBit' :: (Real a) => (a, (Integer, Integer)) -> Bool
testHighestBit' (input, expected) = expected == highestBit input

-- smartReplicate
testSmartReplicate :: Bool
testSmartReplicate = testIt "smartReplicate" testSmartReplicate' [ ([1::Int, 2, 3]
                                                                   , [1, 2, 2, 3, 3, 3])
                                                                 , ([0, 1], [1])
                                                                 ]

testSmartReplicate' :: ([Int], [Int]) -> Bool
testSmartReplicate' (input, expected) = expected == smartReplicate input

-- contains
testContains :: Bool
testContains = testIt "contains" testContains' [ ((3::Integer, [[(1::Integer)..5], [2,0], [3,4]])
                                                 , [[1..5],[3,4]])
                                               ]

testContains' :: (Eq a) => ((a, [[a]]), [[a]]) -> Bool
testContains' ((what, lists), expected) = expected == contains what lists

-- removeAt
testRemoveAt :: Bool
testRemoveAt = testIt "removeAt" testRemoveAt' [ ((1::Int, [1::Integer,2,3]), (Just 2, [1,3]))
                                               , ((10, [1,2,3]), (Nothing, [1,2,3]))
                                               , ((3, [1..5]), (Just 4, [1,2,3,5]))
                                               ]

testRemoveAt2 :: Bool
testRemoveAt2 = testIt "removeAt2" testRemoveAt' [ ((2, "abc"), (Just 'c', "ab"))
                                                 ]

testRemoveAt' :: (Eq a) => ((Int, [a]), (Maybe a, [a])) -> Bool
testRemoveAt' ((idx, ls), expected) = expected == removeAt idx ls

-- collectEvery
testCollectEvery :: Bool
testCollectEvery = testIt "collectEvery" testCollectEvery' [ ((3::Int, [(1::Integer)..8])
                                                             , ([1,2,4,5,7,8], [3,6]))
                                                           ]

testCollectEvery' :: Eq a => ((Int, [a]), ([a], [a])) -> Bool
testCollectEvery' ((k, ls), expected) = expected == collectEvery k ls

-- stringSum
testStringSum :: Bool
testStringSum = testIt "stringSum" testStringSumPass (stringSumPassTests
                                                      ++ stringSumAdvancedTests)
testStringSumPass :: String -> Bool
testStringSumPass input =
    let x = stringSum input
    in x > (-123456)


stringSumPassTests :: [String]
stringSumPassTests = [ "1", "1 2 3", " 1", "1 ", "\t1\t", "\t12345\t", "010 020 030"
                     , " 123 456 789 ", "-1", "-1 -2 -3", "\t-12345\t", " -123 -456 -789 "
                     , "\n1\t\n3   555  -1\n\n\n-5", "123\t\n\t\n\t\n321 -4 -40"
                     ]

stringSumMustFail :: [String]
stringSumMustFail  = ["asd", "1-1", "1.2", "--2", "+1", "1+"]

stringSumAdvancedTests :: [String]
stringSumAdvancedTests    = [ "+1", "1 +1", "-1 +1", "+1 -1"]

stringSumAdvancedMustFail :: [String]
stringSumAdvancedMustFail = ["1+1", "++1", "-+1", "+-1", "1 + 1"]

-- mergeSort
testMergeSort' :: Ord a => [a] -> Bool
testMergeSort' ls = sort ls == mergeSort ls

-- splitOn
testSplitOn :: Bool
testSplitOn = testIt "splitOn" testSplitOn' [(('/', "path/to/file"), ["path", "to", "file"])]

testSplitOn' :: (Eq a, Foldable t) => ((a, t a), [[a]]) -> Bool
testSplitOn' ((sep, ls), expected) = expected == splitOn sep ls

-- joinWith
testJoinWith :: Bool
testJoinWith = testIt "joinWith" testJoinWith' [(('/', ["path", "to", "file"]), "path/to/file")]

testJoinWith' :: (Eq a, Foldable t, Foldable r) => ((a, r (t a)), [a]) -> Bool
testJoinWith' ((sep, ls), expected) = expected == joinWith sep ls

testSplitJoin :: Bool
testSplitJoin = testIt "joinWith . splitOn"
                       (\s -> s == joinWith '/' (splitOn '/' s))
                       [ "path/to/file"
                       , "path/to/dir/"
                       , "/path/from/root/to/file"
                       , "/path/from/root/to/dir"
                       ]

-- consolidated
testAll :: Bool
testAll = and [ testOrder3
              , testHighestBit
              , testSmartReplicate
              , testContains
              , testRemoveAt
              , testRemoveAt2
              , testCollectEvery
              , testStringSum
              , testSplitOn
              , testJoinWith
              , testSplitJoin
              ]

testIt :: (Show a) => String -> (a -> Bool) -> [a] -> Bool
testIt name test = all testIt'
  where
    testIt' input = test input || fail name input

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

fail :: (Show b) => String -> b -> a
fail name test = error (name ++ ": failed on " ++ show test)
