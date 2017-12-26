module Main
       ( main
       ) where

import           Control.Monad      (guard)
import           Control.Monad.Loop (continue, continue_, for, loop)
import           Criterion.Main     (bench, bgroup, defaultConfig, defaultMainWith, nf)
import           Criterion.Types    (reportFile, timeLimit)
import           Data.Foldable      (and, toList)
import           Prelude            hiding (and)


main :: IO ()
main = defaultMainWith (defaultConfig { reportFile = Just "list-vs-loop.html"
                                      , timeLimit = 30
                                      })
   [ bgroup "100 primes"
     [ bench "list" $ nf (take 100) (stupidPrimesList :: [Int])
     , bench "loop" $ nf (take 100) (stupidPrimesLoop :: [Int])
     ]
   , bgroup "555 primes"
     [ bench "list" $ nf (take 555) (stupidPrimesList :: [Int])
     , bench "loop" $ nf (take 555) (stupidPrimesLoop :: [Int])
     ]
   , bgroup "1001 primes"
     [ bench "list" $ nf (take 1001) (stupidPrimesList :: [Int])
     , bench "loop" $ nf (take 1001) (stupidPrimesLoop :: [Int])
     ]
   , bgroup "projectEurler #7"
     [ bench "list" $ nf (take 10001) (stupidPrimesList :: [Int])
     , bench "loop" $ nf (take 10001) (stupidPrimesLoop :: [Int])
     ]
   ]

stupidPrimesList :: [Int]
stupidPrimesList = 2 : do
    p <- [3, 5 ..]
    guard $ and $ do
        d <- takeWhile (\i -> i * i <= p) [3 ..]
        return (p `mod` d /= 0)
    return p

stupidPrimesLoop :: [Int]
stupidPrimesLoop = (2 :) $ toList $ loop $ do
    p <- for 3 (const True) (+ 2)
    if and $ loop $ do
        d <- for 3 (\i -> i * i <= p) (+ 2)
        return (p `mod` d /= 0)
    then continue p
    else continue_


-- projectEuler7 :: Int
-- projectEuler7 = last $ take 10001 $ stupidPrimesList
-- projectEuler7 = last $ take 10001 $ stupidPrimesLoop
