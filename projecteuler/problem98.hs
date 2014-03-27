
import Data.Char (digitToInt)
import System.IO
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

import Util

makeList :: String -> [String]
makeList str = read $ "[" ++ (filter (/= '\\') str) ++ "]"

-- Read file with words
readLines = do
        handle <- openFile "words.txt" ReadMode
        list <- hGetLine handle
        hClose handle
        return list

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

unique :: Eq a => [a] -> [a]
unique [] = []
unique z@(x:xs) = if (any ((==) x) xs) then xs else z 

encodings :: String -> Map Char Int
encodings s = Map.fromList [ (c, d) | c <- unique s, d <- [0..9]]



main = do
	lines <- readLines
	print $ xxx
         where  
               enMaps = map ( encodings . head) anagramSets
			   anagramSets = filter longerThanOne ((anagramClasses . makeList) lines)
               longerThanOne (x:y:xs) = True
               longerThanOne _ = False

