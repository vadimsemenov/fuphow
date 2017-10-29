module Parsers.AParserSpec
       ( spec
       ) where

import           Control.Applicative ((<|>))
import           Data.Char           (chr, isUpper)
import           Test.Hspec

import           Parsers.AParser     (Parser (..), abParser, abParser_, char, first,
                                      intOrUppercase, intPair, posInt, satisfy)


spec :: Spec
spec = do
    describe "satisfy" $
        context "with isUpper" $ do
            let parser = satisfy isUpper
            it "parses \"ABC\"" $
                runParser parser "ABC" `shouldBe` Just ('A', "BC")
            it "fails \"aBC\"" $
                runParser parser "aBC" `shouldBe` Nothing

    describe "char" $
        context "with 'x'" $ do
            let parser = char 'x'
            it "parses \"xyz\"" $
                runParser parser "xyz" `shouldBe` Just ('x', "yz")

    describe "first" $
        it "chr" $
            first chr (48, ()) `shouldBe` ('0', ())

    describe "abParser" $ do
        it "parses \"abcdef\"" $
            runParser abParser "abcdef" `shouldBe` Just (('a', 'b'), "cdef")
        it "fails \"aebcdf\"" $
            runParser abParser "aebcdf" `shouldBe` Nothing

    describe "abParser_" $ do
        it "parses \"abcdef\"" $
            runParser abParser_ "abcdef" `shouldBe` Just ((), "cdef")
        it "fails \"aebcdf\"" $
            runParser abParser_ "aebcdf" `shouldBe` Nothing

    describe "intPair" $ do
        it "parses Int pair" $
            runParser intPair "12 34" `shouldBe` Just ([12, 34], "")
        it "parses only two Ints" $
            runParser intPair "12 34 47" `shouldBe` Just ([12, 34], " 47")
        it "fails on single Int" $
            runParser intPair "42" `shouldBe` Nothing
        it "fails on alphas" $
            runParser intPair "abacaba 42" `shouldBe` Nothing

    describe "instance Alternative" $
        context "with abParser_" $ do
            let failParser    = Parser $ \_ -> error "shouldn't be called"
            let nothingParser = Parser $ const Nothing
            let input         = "abblah"
            let expected      = Just ((), "blah")
            it "doesn't call second parser on first's success" $
                runParser (abParser_ <|> failParser) input `shouldBe` expected
            it "calls second parser on first's fail" $
                runParser (nothingParser <|> abParser_) input `shouldBe` expected

    describe "intOrUppercase" $ do
        it "parses \"342abcd\"" $
            runParser intOrUppercase "342abcd" `shouldBe` Just ((), "abcd")
        it "parses \"XYZ\"" $
            runParser intOrUppercase "XYZ" `shouldBe` Just ((), "YZ")
        it "fails on \"foo\"" $
            runParser intOrUppercase "foo" `shouldBe` Nothing
