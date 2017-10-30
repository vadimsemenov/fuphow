module Parsers.MonstuparSpec
       ( spec
       ) where

import           Test.Hspec

import           Control.Applicative ((<|>))
import           Data.Char           (chr, isUpper)
import           Data.Either         (isLeft)

import           Parsers.Monstupar   (Atom (..), Ident, Monstupar (..), ParseError (..),
                                      SExpr (..), abParser, abParser_, char,
                                      constantFoldingParser, formatLets, ident,
                                      intOrUppercase, intPair, oneOrMore, parseSExpr,
                                      posInt, satisfy, spaces, zeroOrMore)


spec :: Spec
spec = do
    describe "satisfy" $
        context "with isUpper" $ do
            let parser = satisfy isUpper
            it "parses \"ABC\"" $
                runParser parser "ABC" `shouldBe` Right ("BC", 'A')
            it "fails \"aBC\"" $
                runParser parser "aBC" `shouldSatisfy` isLeft

    describe "char" $
        context "with 'x'" $ do
            let parser = char 'x'
            it "parses \"xyz\"" $
                runParser parser "xyz" `shouldBe` Right ("yz", 'x')

    describe "abParser" $ do
        it "parses \"abcdef\"" $
            runParser abParser "abcdef" `shouldBe` Right ("cdef", ('a', 'b'))
        it "fails \"aebcdf\"" $
            runParser abParser "aebcdf" `shouldSatisfy` isLeft

    describe "abParser_" $ do
        it "parses \"abcdef\"" $
            runParser abParser_ "abcdef" `shouldBe` Right ("cdef", ())
        it "fails \"aebcdf\"" $
            runParser abParser_ "aebcdf" `shouldSatisfy` isLeft

    describe "intPair" $ do
        it "parses Int pair" $
            runParser intPair "12 34" `shouldBe` Right ("", [12, 34])
        it "parses only two Ints" $
            runParser intPair "12 34 47" `shouldBe` Right (" 47", [12, 34])
        it "fails on single Int" $
            runParser intPair "42" `shouldSatisfy` isLeft
        it "fails on alphas" $
            runParser intPair "abacaba 42" `shouldSatisfy` isLeft

    describe "instance Alternative" $
        context "with abParser_" $ do
            let failParser = Monstupar $ \_ -> error "shouldn't be called"
            let leftParser = Monstupar $ const (Left $ ParseError "nothing")
            let input      = "abblah"
            let expected   = Right ("blah", ())
            it "doesn't call second parser on first's success" $
                runParser (abParser_ <|> failParser) input `shouldBe` expected
            it "calls second parser on first's fail" $
                runParser (leftParser <|> abParser_) input `shouldBe` expected

    describe "intOrUppercase" $ do
        it "parses \"342abcd\"" $
            runParser intOrUppercase "342abcd" `shouldBe` Right ("abcd", ())
        it "parses \"XYZ\"" $
            runParser intOrUppercase "XYZ" `shouldBe` Right ("YZ", ())
        it "fails on \"foo\"" $
            runParser intOrUppercase "foo" `shouldSatisfy` isLeft

    describe "zeroOrMore" $
        context "with satisfy isUpper" $ do
            let parser = satisfy isUpper
            it "parses \"ABCdEfgH\"" $
                runParser (zeroOrMore parser) "ABCdEfgH" `shouldBe`
                    Right ("dEfgH", "ABC")
            it "parses (nothing) \"abcdeFGh\"" $
                runParser (zeroOrMore parser) "abcdeFGh" `shouldBe`
                    Right ("abcdeFGh", "")

    describe "oneOrMore" $
        context "with satisfy isUpper" $ do
            let parser = satisfy isUpper
            it "parses \"ABCdEfgH\"" $
                runParser (oneOrMore parser) "ABCdEfgH" `shouldBe`
                    Right ("dEfgH", "ABC")
            it "fails on \"abcdeFGh\"" $
                runParser (oneOrMore parser) "abcdeFGh" `shouldSatisfy` isLeft

    describe "spaces" $ do
        it "parses zero spaces" $
            runParser spaces "abacaba" `shouldBe` Right ("abacaba", "")
        it "parses sequence of spaces (and tabs, and eols...)" $
            runParser spaces "  \t \n\t abacaba" `shouldBe`
                Right ("abacaba", "  \t \n\t ")

    describe "ident" $ do
        it "parses only one ident" $
            runParser ident "foobar baz" `shouldBe`
                Right (" baz", "foobar" :: Ident)
        it "parses single ident" $
            runParser ident "foo33fA" `shouldBe` Right ("", "foo33fA" :: Ident)
        it "fails on \"2bad\"" $
            runParser ident "2bad" `shouldSatisfy` isLeft
        it "fails on empty input" $
            runParser ident "" `shouldSatisfy` isLeft

    describe "parseSExpr" $ do
        context "with sample valid S-expression" $ do
            it "parses \"5\"" $
                runParser parseSExpr "5" `shouldBe` Right ("", A (N 5))
            it "parses \"foo3\"" $
                runParser parseSExpr "foo3" `shouldBe` Right ("", A (I "foo3"))
            it "parses \"(bar (foo) 3 5 874)\"" $
                runParser parseSExpr "(bar (foo) 3 5 874)" `shouldBe`
                    Right ( ""
                          , Comb [A (I "bar")
                                 , Comb [A (I "foo")]
                                 , A (N 3)
                                 , A (N 5)
                                 , A (N 874)
                                 ]
                          )
            it "parses\"(((lambda x (lambda y (plus x y))) 3) 5)\"" $
                runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)" `shouldBe`
                    Right ( ""
                          , Comb [ Comb [ Comb [ A (I "lambda")
                                               , A (I "x")
                                               , Comb [ A (I "lambda")
                                                      , A (I "y")
                                                      , Comb [ A (I "plus")
                                                             , A (I "x")
                                                             , A (I "y")
                                                             ]
                                                      ]
                                               ]
                                        , A (N 3)
                                        ]
                                 , A (N 5)
                                 ]
                          )

            it "parses \"( lots of ( spaces in ) this ( one ) )\"" $
                runParser parseSExpr "( lots of ( spaces in ) this ( one ) )" `shouldBe`
                Right ( ""
                      , Comb [ A (I "lots")
                             , A (I "of")
                             , Comb [ A (I "spaces")
                                    , A (I "in")
                                    ]
                             , A (I "this")
                             , Comb [ A (I "one") ]
                             ]
                      )


        it "fails on illegal Indent" $
            runParser parseSExpr "(((lambda x (lambda y (plus x 1IllegalInden))) 3) 5)" `shouldSatisfy` isLeft

        it "parses only valid prefix" $
            runParser parseSExpr "(foo (bar 8) baz) not SExpr" `shouldBe`
                Right ( "not SExpr"
                      , Comb [ A (I "foo")
                             , Comb [ A (I "bar")
                                    , A (N 8)
                                    ]
                             , A (I "baz")
                             ]
                      )


    describe "constantFoldingParser" $ do
        it "simple let" $
            runParser constantFoldingParser "let x = 5"
                `shouldBe` Right ("", [("x", 5)])
        it "dependent lets" $
            runParser constantFoldingParser " let\t x   =5\n\
                                            \let   y=x+5 \t +  x  "
                `shouldBe` Right ("", [ ("x", 5)
                                      , ("y", 15)])
        it "sample" $
            runParser constantFoldingParser "let x = 1 + 2 + 5\n\
                                            \let   y = x+x  \t\n\
                                            \let z=0+    x   + y + 8"
                `shouldBe` Right ("", [ ("x", 8)
                                      , ("y", 16)
                                      , ("z", 32)])

        it "fails on two lets on single line" $
            runParser constantFoldingParser "let x = 2 let y = 3"
                `shouldSatisfy` isLeft
        it "fails on forward definition" $
            runParser constantFoldingParser "let x = y\n\
                                            \let y = 100500" `shouldSatisfy` isLeft
        it "parses fails on multiline let" $
            runParser constantFoldingParser "let x = 2 +\n\
                                            \3 + 4\n\
                                            \let y = x" `shouldSatisfy` isLeft

    describe "formatLets" $ do
        it "trims all useless spaces" $
            (formatLets . snd) <$> runParser constantFoldingParser
                                             "   let  \tx  =  \t3   + \t\t  4 "
                `shouldBe` Right "let x = 7\n"
        it "indents =" $
            (formatLets . snd) <$> runParser constantFoldingParser
                                             "  let \tx=3+3+3+3\n\
                                             \ let     foo \t=1 +x  "
                `shouldBe` Right "let x   = 12\n\
                                 \let foo = 13\n"
