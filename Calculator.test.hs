import Test.HUnit
import Test.HUnit.Plus
import Calculator

calculatorTests = test [
        "empty string -> 0" ~: "not 0" ~: 0 ~=? add "",
        "one number -> the number" ~: "single number" ~: 2 ~=? add "2",
        "two numbers -> sum of numbers" ~: "two numbers" ~: 3 ~=? add "2,1",
        "many numbers -> sum of numbers" ~: "many numbers" ~: 10 ~=? add "2,1,4,3",
        "Numbers can be separated with newlines" ~: "use newlines" ~: 3 ~=? add "2\n1",
        "Numbers can be separated also with newlines" ~: "use newlines" ~: 10 ~=? add "2\n1, 4\n3",
        "Delimiter can be customised" ~: "custom delimiter" ~: 3 ~=? add "//;\n1;2",
        "Negative number throws exception" ~: "negative number" ~: 
    ]