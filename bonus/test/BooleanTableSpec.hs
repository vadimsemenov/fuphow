module BooleanTableSpec
       (spec
       ) where

import           Test.Hspec

import           BooleanTable (TruthTable (..))


spec :: Spec
spec = describe "TruthTable" $ do
    it "\a b -> a && (a || b)" $
        truthTable (\a b -> a && (a || b)) `shouldBe` [ [True,  True,   True]
                                                      , [True,  False,  True]
                                                      , [False, True,   False]
                                                      , [False, False,  False]
                                                      ]
    it "\a b c -> a || b || not c" $
        truthTable (\a b c -> a || b || not c) `shouldBe` [ [True , True , True ,  True]
                                                          , [True , True , False,  True]
                                                          , [True , False, True ,  True]
                                                          , [True , False, False,  True]
                                                          , [False, True , True ,  True]
                                                          , [False, True , False,  True]
                                                          , [False, False, True ,  False]
                                                          , [False, False, False,  True]
                                                          ]
