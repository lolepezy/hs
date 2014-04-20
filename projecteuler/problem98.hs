
import Data.Char (digitToInt)
import System.IO
import qualified Data.List as List
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M

import Util

makeList :: String -> [String]
makeList str = read $ "[" ++ (filter (/= '\\') str) ++ "]"

-- Read file with words
readLines = do
        handle <- openFile "words.txt" ReadMode
        list <- hGetLine handle
        hClose handle
        return list

anagramClasses :: [String] -> [[String]]
anagramClasses [] = []
anagramClasses (w:words) = (w:anagrams) : anagramClasses notAnagrams 
    where (anagrams, notAnagrams) = List.partition (anagram w) words
          anagram word1 word2     = List.sort word1 == List.sort word2

type EncMap = Map Char Int 

encodingMappings :: String -> [EncMap]
encodingMappings word = encMap word	(M.empty :: EncMap)
  where
    encMap :: String -> EncMap -> [EncMap]
    encMap [] mapping = [mapping]
    encMap (c : cs) mapping = case M.lookup c mapping of 
      Just digit -> encMap cs mapping
      Nothing -> concatMap subMappings feasibleDigits
        where
          subMappings digit = encMap cs (M.insert c digit mapping)
          feasibleDigits = filter (\d -> notElem d (M.elems mapping)) [0..9]

encode :: EncMap -> String -> Maybe Int
encode mapping word = case map (mappingToEnc mapping) word of
      0 : _ -> Nothing
      d @ _ -> Just (read (concatMap show d) :: Int)
  where
    mappingToEnc mapping char = case M.lookup char mapping of
      Just digit -> digit

squareEncodings :: String -> [(EncMap, Int)]
squareEncodings s = [ (encMap, num) | (encMap, num) <- encodings s, (isJust . intSqrt) num ]
  where
    encodings s = [ (m, code) | (m, Just code) <- map (\m -> (m, encode m s)) $ encodingMappings s ]


squareAnagramClass :: [String] -> Int
squareAnagramClass [] = 0
squareAnagramClass (word : ws) = if null otherSquares then 0 else maximum otherSquares
  where 
	otherSquares = [ max num otherNum | (encMap, num) <- squareEncodings word, Just otherNum <- map (encode encMap) ws, (isJust . intSqrt) otherNum ]


main = do
  lines <- readLines  
  --print $ map (map encodings) $ filter ((>1) . length) $ anagramClasses $ makeList lines
  --print $ filter ((>1) . length) $ anagramClasses $ makeList lines
  print $ squareEncodings "ABB"
  let classes = filter ((>1) . length) $ anagramClasses $ makeList lines
  print $ classes
  --print $ squareEncodings $ head $ head $ classes	
  print $ maximum $ map squareAnagramClass $ classes

