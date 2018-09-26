module Calculator(add) where 

import Data.List
import Data.List.Split
import Text.Regex.Posix

add :: String -> Int
add "" = 0
add ('/':'/':rest) = 
    let (_,_,s,[delims]) = rest =~ "(.)\n" :: (String, String, String,[String])
    in add' delims s
add s = add' "," s


add' :: String -> String -> Int
add' delims s = sum . map read $ splitOneOf (delims ++ "\n") s

-- Prelude Text.Regex.Posix> "//,\n1,2\n3" =~ "//(.)\n" :: (String,String,String,[String])
-- ("", "//,\n", "1,2\n3", [","])