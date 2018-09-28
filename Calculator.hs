module Calculator(add) where 

import Data.Char
import Data.List
import Data.List.Split
import Text.Regex.PCRE

add :: String -> Int
add input
    | input == "" = 0
    | "//[" `isPrefixOf` input = 
        let delims = extractLongDelims input
            values = (lines input) !! 1
        in parseAndSum $ splitOnLongDelims delims values
    | (_,_,values,[delim]) <- input =~ "//(.)\n" :: (String, String, String,[String]) =  
        parseAndSum $ splitOneOf ('\n':delim) values
    | otherwise = parseAndSum $ splitOneOf "\n," input

parseAndSum :: [String] -> Int
parseAndSum strings
    | null negatives = sum $ filter (\n -> n < 1001 ) numbers
    | otherwise = raiseError negatives
    where 
        -- trimmed = [(dropWhile isSpace s) | s <- strings]
        -- negatives = [s | s <- trimmed, head s == '-']
        -- numbers = map read trimmed

        -- trimmed = map (\s -> dropWhile isSpace s) strings
        -- negatives = filter (\s -> head s == '-') trimmed
        -- numbers = map read trimmed

        -- negatives = filter (\s -> head s == '-') $ map (\s -> dropWhile isSpace s) strings
        -- numbers = map read strings

        negatives = [s2 | s2 <- [(dropWhile isSpace s1) | s1 <- strings], head s2 == '-']
        numbers = map read strings

raiseError :: [String] -> Int
raiseError negatives =
    let string = intercalate ", " negatives
    in error $ "negatives not allowed ==> " ++ string

extractLongDelims :: String -> [String]
extractLongDelims string =
    map (\i -> i !! 2) (string =~ "(\\[(.*?)])+?")

splitOnLongDelims :: [String] -> String -> [String]
splitOnLongDelims delims values
    | [delim] <- delims =  
        splitOn delim values
    | otherwise = 
        foldl (\ values delim -> splitAndFlatten delim values) [values] delims
        where splitAndFlatten delim values = concat [splitOn delim v | v <- values]