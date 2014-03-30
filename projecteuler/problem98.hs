
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


type EncMap = Map Char Int 

encodings :: String -> [[Int]]
encodings [] = []
encodings s = encMap s (Map.fromList [] :: EncMap)
  where
    encMap :: String -> EncMap -> [[Int]]
    encMap [] _ = [[]]
    encMap (c : cs) mapping = case Map.lookup c mapping of 
      Just digit -> map (digit : ) (encMap cs mapping)
      Nothing -> flatten $ map makeOptions $ filter (\d -> not (d `elem` (Map.elems mapping))) [0..9]
        where
          makeOptions digit = [ digit : rest | rest <- encMap cs (Map.insert c digit mapping)] 
          flatten list = foldr (++) [] list

squareEncodings s = filter isSquare $ map number (encodings s)
  where
    number :: [Int] -> Int
    number digits = read (foldr (++) "" (map show digits)) :: Int
    isSquare d = case intSqrt d of
      Nothing -> False
      Just _ -> True

maxSquare :: [String] -> Int
maxSquare words = maximum $ foldr (++) [] $ map (\ac -> map squareEncodings ac) (anagramClasses words) 

main = do
  lines <- readLines  
  print $ maxSquare $ makeList lines
