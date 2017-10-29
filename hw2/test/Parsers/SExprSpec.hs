module Parsers.SExprSpec
       ( spec
       ) where

import           Test.Hspec

import           Data.Char       (isUpper)

import           Parsers.AParser (Parser (..), satisfy)
import           Parsers.SExpr   (Atom (..), Ident, SExpr (..), ident, oneOrMore,
                                  parseSExpr, spaces, zeroOrMore)


spec :: Spec
spec = do
    describe "zeroOrMore" $
        context "with satisfy isUpper" $ do
            let parser = satisfy isUpper
            it "parses \"ABCdEfgH\"" $
                runParser (zeroOrMore parser) "ABCdEfgH" `shouldBe` Just ("ABC", "dEfgH")
            it "parses (nothing) \"abcdeFGh\"" $
                runParser (zeroOrMore parser) "abcdeFGh" `shouldBe` Just ("", "abcdeFGh")

    describe "oneOrMore" $
        context "with satisfy isUpper" $ do
            let parser = satisfy isUpper
            it "parses \"ABCdEfgH\"" $
                runParser (oneOrMore parser) "ABCdEfgH" `shouldBe` Just ("ABC", "dEfgH")
            it "fails on \"abcdeFGh\"" $
                runParser (oneOrMore parser) "abcdeFGh" `shouldBe` Nothing

    describe "spaces" $ do
        it "parses zero spaces" $
            runParser spaces "abacaba" `shouldBe` Just ("", "abacaba")
        it "parses sequence of spaces (and tabs, and eols...)" $
            runParser spaces "  \t \n\t abacaba" `shouldBe` Just ("  \t \n\t ", "abacaba")

    describe "ident" $ do
        it "parses only one ident" $
            runParser ident "foobar baz" `shouldBe` Just ("foobar" :: Ident, " baz")
        it "parses single ident" $
            runParser ident "foo33fA" `shouldBe` Just ("foo33fA" :: Ident, "")
        it "fails on \"2bad\"" $
            runParser ident "2bad" `shouldBe` Nothing
        it "fails on empty input" $
            runParser ident "" `shouldBe` Nothing

    describe "parseSExpr" $ do
        context "with sample valid S-expression" $ do
            it "parses \"5\"" $
                runParser parseSExpr "5" `shouldBe` Just (A (N 5), "")
            it "parses \"foo3\"" $
                runParser parseSExpr "foo3" `shouldBe` Just (A (I "foo3"), "")
            it "parses \"(bar (foo) 3 5 874)\"" $
                runParser parseSExpr "(bar (foo) 3 5 874)" `shouldBe` Just ( Comb [A (I "bar")
                                                                                  , Comb [A (I "foo")]
                                                                                  , A (N 3)
                                                                                  , A (N 5)
                                                                                  , A (N 874)
                                                                                  ]
                                                                           , "")
            it "parses\"(((lambda x (lambda y (plus x y))) 3) 5)\"" $
                runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)" `shouldBe`
                    Just ( Comb [ Comb [ Comb [ A (I "lambda")
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
                         , "")

            it "parses \"( lots of ( spaces in ) this ( one ) )\"" $
                runParser parseSExpr "( lots of ( spaces in ) this ( one ) )" `shouldBe`
                Just ( Comb [ A (I "lots")
                            , A (I "of")
                            , Comb [ A (I "spaces")
                                   , A (I "in")
                                   ]
                            , A (I "this")
                            , Comb [ A (I "one") ]
                            ]
                     , "")


        it "fails on illegal Indent" $
            runParser parseSExpr "(((lambda x (lambda y (plus x 1IllegalInden))) 3) 5)" `shouldBe` Nothing

        it "parses only valid prefix" $
            runParser parseSExpr "(foo (bar 8) baz) not SExpr" `shouldBe` Just ( Comb [ A (I "foo")
                                                                                      , Comb [ A (I "bar")
                                                                                             , A (N 8)
                                                                                             ]
                                                                                      , A (I "baz")
                                                                                      ]
                                                                               , "not SExpr")
