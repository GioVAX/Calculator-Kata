module Calculator(add) where 

import Data.List
import Data.List.Split
import Text.Regex.Posix

add :: String -> Int
add s
    | s == "" = 0
    | (_,_,values,[delims]) <- s =~ "//\\[(.*)]\n" :: (String, String, String,[String]) =
    -- let (_,_,s,[delims]) = ('[':rest) =~ "((?:\\[(.*)])+)\n" :: (String, String, String,[String])
        add' $ splitOn delims values
    | (_,_,values,[delims]) <- s =~ "//(.)\n" :: (String, String, String,[String]) =  
        add' $ splitOneOf ('\n':delims) values
    | otherwise = add' $ splitOneOf "\n," s


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

-- Prelude> "//,\n1,2\n3" =~ "//(.)\n" :: (String,String,String,[String])
-- ("", "//,\n", "1,2\n3", [","])

-- Prelude> ('[':"*as][%ss]\n1**2%%3") =~ "(\\[(.*)])\n" :: (String, String, String,[String])
-- ("","[*as][%ss]\n","1**2%%3",["[*as][%ss]","*as][%ss"])

-- Prelude> "*as][%ss][123]\n1**2%%3" =~ "((.*))]\n" :: (String, String, String,[String])
-- ("","*as][%ss][123]\n","1**2%%3",["*as][%ss][123","*as][%ss][123"])
-- "*as][%ss][123" can be split on "][" to get the list of delimiters