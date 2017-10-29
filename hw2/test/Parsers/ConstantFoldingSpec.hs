module Parsers.ConstantFoldingSpec
       ( spec
       ) where

import           Test.Hspec

import           Parsers.ConstantFolding (formatLets, parse)


spec :: Spec
spec = do
    describe "parse" $ do
        it "simple let" $
            parse "let x = 5" `shouldBe` Just [("x", 5)]
        it "dependent lets" $
            parse " let\t x   =5\n\
                  \let   y=x+5 \t +  x  " `shouldBe` Just [ ("x", 5)
                                                          , ("y", 15)]
        it "sample" $
            parse "let x = 1 + 2 + 5\n\
                  \let   y = x+x  \t\n\
                  \let z=0+    x   + y + 8" `shouldBe` Just [ ("x", 8)
                                                            , ("y", 16)
                                                            , ("z", 32)]

        it "fails on two lets on single line" $
            parse "let x = 2 let y = 3" `shouldBe` Nothing
        it "fails on forward definition" $
            parse "let x = y\n\
                  \let y = 100500" `shouldBe` Nothing
        it "parses fails on multiline let" $
            parse "let x = 2 +\n\
                  \3 + 4\n\
                  \let y = x" `shouldBe` Nothing

    describe "formatLets" $ do
        it "trims all useless spaces" $
            formatLets <$> parse "   let  \tx  =  \t3   + \t\t  4 " `shouldBe`
                Just "let x = 7\n"
        it "indents =" $
            formatLets <$> parse "  let \tx=3+3+3+3\n\
                                   \ let     foo \t=1 +x  " `shouldBe`
                Just "let x   = 12\n\
                     \let foo = 13\n"
