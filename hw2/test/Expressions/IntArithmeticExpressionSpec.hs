module Expressions.IntArithmeticExpressionSpec
       ( spec
       ) where

import           Test.Hspec

import           Expressions.IntArithmeticExpression (IntArithmeticError
                                                     , IntExpr (..)
                                                     , evalInt)

spec :: Spec
spec = do
    describe "random expressions" $
        context "when a = 2, b = 3, c = 4, d = 12" $ do
            let [a', b', c', d'] = [2 :: Int, 3, 4, 12]
            let [a, b, c, d] = map Const [a', b', c', d']
            it "a + ((b * c) - d)" $ do
                let res = a' + (b' * c' - d')
                evalInt (Sum a (Sub (Mul b c) d)) `shouldBe` Right res
            it "a * ((b * c) `div` d)" $ do
                let res = a' * ((b' * c') `div` d')
                evalInt (Mul a (Div (Mul b c) d)) `shouldBe` Right res
            it "(a ^ b) * (d / c)" $ do
                let res = (a' ^ b') * (d' `div` c')
                evalInt (Mul (Pow a b) (Div d c)) `shouldBe` Right res

    describe "div" $
        context "when a = 2, b = 3, c = 5, d = 6, e = -5, f = -2" $ do
            let [a', b', c', d', e', f'] = [2 :: Int, 3, 5, 6, -5, -2]
            let [a, b, c, d, e, f] = map Const [a', b', c', d', e', f']
            it "integral division" $
                evalInt (Div d a) `shouldBe` evalInt b
            it "integral division by negative" $
                evalInt (Div d f) `shouldBe` evalInt (Sum a e)
            it "nonintegral division" $
                evalInt (Div c b) `shouldBe` Right (c' `div` b')
            it "nonintegral division by negative" $
                evalInt (Div c f) `shouldBe` Right (c' `div` f')
            it "division of negative" $
                evalInt (Div e f) `shouldBe` Right (e' `div` f')
            it "division by zero" $
                evalInt (Div e (Const 0)) `shouldBe` Left "Division by zero"

    describe "pow" $
        context "when a = 2, b = 3, c = -2" $ do
            let [a', b', c'] = [2 :: Int, 3, -2]
            let [a, b, c] = map Const [a', b', c']
            it "a ^ b" $
                evalInt (Pow a b) `shouldBe` Right (a' ^ b')
            it "[negative base] c ^ a" $
                evalInt (Pow c a) `shouldBe` Right (c' ^ a')
            it "[negative base] c ^ b" $
                evalInt (Pow c b) `shouldBe` Right (c' ^ b')
            it "a ^ 0 == 1" $
                evalInt (Pow a (Sub b b)) `shouldBe` Right (1 :: Int)
            it "a ^ c is NaN" $
                evalInt (Pow a c) `shouldBe` Left "Negative exponent"
--            it "0 ^ 0 is NaN" $ do
--                evalInt (Pow (Sub a a) (Sub c c)) `shouldBe` Left "0^0 is NaN"
