module Calculator.Tests where 

import Test.Hspec
import Calculator
import Control.Exception.Base

main :: IO ()
main = hspec $ do
    describe "Calculator add" $ do
        it "returns 0 for empty string" $
            add "" `shouldBe` 0

        it "returns the number for one number input" $
            add "5" `shouldBe` 5

        it "returns sum of the input numbers" $
            add "5,2" `shouldBe` 7

        it "returns the sum of multiple input numbers" $
            add "2,1,4,3" `shouldBe` 10

        it "returns the sum of input numbers also separated with newlines" $
            add "2\n1,4,5\n3" `shouldBe` 15

        it "allows to customise the delimiter" $
            add "//;\n1;2" `shouldBe` 3

        it "allows to customise the delimiter and use newline to separate numbers" $
            add "//;\n1;2\n4\n6;2" `shouldBe` 15

        it "throws exception when given negative number" $
            evaluate (add "2,-3, 4") `shouldThrow` anyException

        it "ignores numbers greater than 1000" $
            add "2, 1001, 3" `shouldBe` 5

        it "ignores numbers greater than 1000 but accepts 1000" $
            add "2, 1000, 3" `shouldBe` 1005
        
        it "allows one long delimiter" $
            add "//[***]\n1***2***3" `shouldBe` 6

        it "allows using multiple long delimiters" $
            add "//[**][%%]\n1**2%%3" `shouldBe` 6