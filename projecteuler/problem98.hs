
import Data.Char (digitToInt)
import System.IO
import Data.Map (Map)
import qualified Data.Map as Map

makeList :: String -> [String]
makeList str = read $ "[" ++ (filter (\c -> c /= '\\') str) ++ "]"

-- Read file with words
readLines = do
        handle <- openFile "words.txt" ReadMode
        list <- hGetLine handle
        hClose handle
        return list

contains :: Eq a => [a] -> a -> Bool
contains [] x = False
contains (y:xs) x = x == y || contains xs x

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = if contains xs x then unique xs else (x:unique xs)

makeList :: String -> [String]
makeList str = read $ "[" ++ (filter (\c -> c /= '\\') str) ++ "]"

equalSets :: Eq a => [a] -> [a] -> Bool
equalSets x y = nested x y && nested y x
    where nested :: Eq b => [b] -> [b] -> Bool
          nested [] _ = True
          nested ps [] = False
          nested (p:ps) qs = any ((==) p) qs && nested ps qs

anagramClasses :: [String] -> [[String]]
anagramClasses [] = []
anagramClasses (w:words) = (w:anagrams) : anagramClasses notAnagrams 
    where (anagrams, notAnagrams) = List.partition (anagram w) words
          anagram word1 word2     = length word1 == length word2 && equalSets word1 word2 


--main = print $ head (makeList "\"CARE\",\"ABOUT\",\"ABOVE\",\"ABSENCE\",\"ABSOLUTELY\"")
main = print $ anagramClasses ["CARE", "RACE"]



main = do
	lines <- readLines
	print $ head (tail (makeList lines))
