
import Data.Char (digitToInt)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

import Util

contains :: Eq a => [a] -> a -> Bool
contains [] x = False
contains (y:xs) x = x == y || contains xs x

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = if contains xs x then unique xs else (x:unique xs)

makeList :: String -> [String]
makeList str = read $ "[" ++ (filter ('\\' /=) str) ++ "]"

equalSets :: Eq a => [a] -> [a] -> Bool
equalSets x y = nested x y && nested y x
    where nested :: Eq b => [b] -> [b] -> Bool
          nested [] _ = True
          nested ps [] = False
          nested (p:ps) qs = any ((==) p) qs && nested ps qs

data AnagramClass = AnagramClass [[String]] deriving Show

anagramClasses :: [String] -> AnagramClass
anagramClasses [] = AnagramClass []
anagramClasses (w : words) = AnagramClass ((w:anagrams) : anagrams1)
    where AnagramClass anagrams1 = anagramClasses others 
          (anagrams, others) = List.partition (anagram w) words
          anagram word1 word2     = length word1 == length word2 && equalSets word1 word2 

type EncMap = Map Char Int 

encodings :: String -> [[Int]]
encodings [] = []
encodings s = encMap s (Map.fromList [] :: EncMap)
  where
    encMap :: String -> EncMap -> [[Int]]
    encMap [] _ = [[]]
    encMap (c : cs) mapping = case Map.lookup c mapping of 
      Just digit -> map (digit : ) (encMap cs mapping)
      Nothing -> flatten $ map makeOptions $ filter (\d -> not (contains (Map.elems mapping) d)) [0..9]
        where
          makeOptions digit = [ digit : rest | rest <- encMap cs (Map.insert c digit mapping)] 
          flatten list = foldr (++) [] list
 
 

--main = print $ head (makeList "\"CARE\",\"ABOUT\",\"ABOVE\",\"ABSENCE\",\"ABSOLUTELY\"")
--main = print $ anagramClasses ["CARE", "RACE", "ACRE", "BARC", "CRAB"]

--main = print $ map intSqrt [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 144]
main = print $ encodings "AABB"

--equalSets [1, 2, 2] [2, 2, 1, 2]



--
