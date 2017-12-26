{-# LANGUAGE TemplateHaskell #-}

module TemplateLib
       ( chooseByIndices
       , OverloadedShow (..)
       , deriveOverloadedShow
       ) where


import           Language.Haskell.TH

import           Data.List           (sort)
import qualified Data.Map            as Map
import qualified Data.Text           as T


chooseByIndices :: Int -> [Int] -> Q Exp
chooseByIndices len indices = do
    let unique :: [Int]
        unique = nub indices
    names <- mapM (newName . ('i' :) . show) unique
    let mapping :: Map.Map Int Name
        mapping = Map.fromList (zip unique names)
    let list :: [PatQ]
        list = [maybe wildP varP x | x <- map (mapping Map.!?) [0 .. len - 1]]
    let result :: [ExpQ]
        result = map (varE . (mapping Map.!)) indices
    lamE [tupP list] (tupE result)


nub :: Ord a => [a] -> [a]
nub = nub' . sort
  where
    nub' [] = []
    nub' [a] = [a]
    nub' (a : b : rest) = if a == b
                          then nub (b : rest)
                          else a : nub (b : rest)


class OverloadedShow a where
    showText :: a -> T.Text

deriveOverloadedShow :: Name -> Q [Dec]
deriveOverloadedShow name = [d|
                              instance OverloadedShow $(conT name) where
                                  showText = T.pack . show
                            |]
