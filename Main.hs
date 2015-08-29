module Main where

import Data.Char (toLower)

-- | The main entry point.
main :: IO ()
main = do
    print "Version 1"
    print (isPalindrome1 "Test")
    print (isPalindrome1 "TesseT")
    print (isPalindrome1 "T")
    print (isPalindrome1 "")
    print (isPalindrome1 "t1t")
    print (isPalindrome1 "Madam I'm Adam")
    print "Version 2"
    print (isPalindrome1 "Test")
    print (isPalindrome2 "TesseT")
    print (isPalindrome2 "T")
    print (isPalindrome2 "")
    print (isPalindrome2 "t1t")
    print (isPalindrome2 "Madam I'm Adam")
    

isPalindrome1 :: String -> Bool
isPalindrome1 "" = True
isPalindrome1 [x] = True
isPalindrome1 x | firstElem /= lastElem = False
                | firstElem == lastElem = isPalindrome1 (tail (init x))
                where firstElem = head x
                      lastElem = last x

isPalindrome2 :: String -> Bool
isPalindrome2 x = x' == x''
                    where x' = normalise x
                          x'' = reverse (normalise x)

normalise :: String -> String
normalise = removePunct . removeSpaces . (map toLower)

removeSpaces :: String -> String
removeSpaces = filter (/= ' ')

removePunct :: String -> String
removePunct = filter (\c -> c `elem` ['a' .. 'z'])
