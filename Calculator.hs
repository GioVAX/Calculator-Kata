module Calculator(add) where 

import Data.List
import Data.List.Split
import Text.Regex.PCRE

add :: String -> Int
add input
    | input == "" = 0
    | "//[" `isPrefixOf` input = 
        let delims = extractLongDelims "(\\[(.*?)])+?" input
            values = (lines input) !! 1
        in add' $ splitOn (head delims) values
    | (_,_,values,[delims]) <- input =~ "//(.)\n" :: (String, String, String,[String]) =  
        add' $ splitOneOf ('\n':delims) values
    | otherwise = add' $ splitOneOf "\n," input


add' :: [String] -> Int
add' strings
    | null negatives = sum $ filter (\n -> n < 1001 ) numbers
    | otherwise = raiseError negatives
    where 
        numbers = map read strings
        negatives = [n | n <- numbers, n < 0]

raiseError :: [Int] -> Int
raiseError numbers =
    let string = intercalate ", " $ map show numbers
    in error $ "negatives not allowed ==> " ++ string

extractLongDelims :: String -> String -> [String]
extractLongDelims regex string =
    map (\i -> i !! 2)  (string =~ regex :: [[String]])


-- map (\i -> i !! 1)  ("//[***]\n1***2***3" =~ "\\[(.*?)]" :: [[String]])
-- ["***"]

-- map (\i -> i !! 1)  ("//[***]\n1***2***3" =~ "(\\[(.*?)])+?" :: [[String]])
-- ["[***]"]



-- map (\i -> i !! 2)  ("//[***]\n1***2***3" =~ "(\\[(.*?)])+?" :: [[String]])
-- ["***"]

-- map (\i -> i !! 2)  ("//[***][$$$]\n1***2***3" =~ "(\\[(.*?)])+?" :: [[String]])
-- ["***","$$$"]