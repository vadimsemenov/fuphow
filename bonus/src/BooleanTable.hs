{-# LANGUAGE FlexibleInstances #-}

module BooleanTable
       ( TruthTable (..)
       , showTruthTable
       , booleanTable
       ) where


class TruthTable f where
    truthTable :: f -> [[Bool]]

instance TruthTable Bool where
    truthTable b = [[b]]

instance (TruthTable f) => TruthTable (Bool -> f) where
    truthTable f = [True, False] >>= \b -> truthTable (f b) >>= \r -> [b : r]

showTruthTable :: [[Bool]] -> String
showTruthTable [] = ""
showTruthTable (r : rs) = showTruthRow r ++ "\n" ++ showTruthTable rs
  where
    showTruthRow []       = error "Missed result"
    showTruthRow [b]      = "= " ++ showBool b
    showTruthRow (b : bs) = showBool b ++ " " ++ showTruthRow bs
    showBool True  = "True "
    showBool False = "False"

booleanTable :: TruthTable f => f -> IO ()
booleanTable f = putStr $ showTruthTable $ truthTable f
