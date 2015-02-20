{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

import Test.QuickCheck
import Test.HUnit
import Data.List (elem)
import Data.Set (toList, fromList)
import Control.Monad.STM

data TreeMap k v = Empty 
  | Leaf k v 
  | Node k v (TreeMap k v) (TreeMap k v)
  deriving (Eq, Show)

emptyMap :: TreeMap k v
emptyMap = Empty

insert :: (Eq k, Ord k) => TreeMap k v -> k -> v -> TreeMap k v
insert Empty key value = Leaf key value
insert (Leaf key value) key' value' 
  | key' < key = Node key value (Leaf key' value') Empty
  | key' == key = Leaf key value'
  | otherwise = Node key value Empty (Leaf key' value')

insert (Node key value left right) key' value' 
  | key' < key = Node key value (replaceSubTree left key' value') right
  | key' == key = Node key value' left right
  | otherwise = Node key value left (replaceSubTree right key' value')
  where
    replaceSubTree Empty key value = Leaf key value
    replaceSubTree tree key value = insert tree key value

get :: (Eq k, Ord k) => TreeMap k v -> k -> Maybe v
get Empty _ = Nothing
get (Leaf key value) key' 
  | key == key' = Just value 
  | otherwise   = Nothing

get (Node key value left right) key' 
  | key' < key  = get left key'
  | key' == key = Just value
  | key' > key  = get right key'
  | otherwise   = Nothing


propMapInsertGet :: [Int] -> [String] -> Bool
propMapInsertGet key value = let 
    m = insert emptyMap key value
  in case get m key of
     Just v -> v == value
     Nothing -> False 

propMapInsertGetBatch :: [(String, Int)] -> Bool
propMapInsertGetBatch keysValues = snd $ foldr  
  (\(key, value) (mm, flag) -> let mx = insert mm key value 
                                   f = case get mx key of
                                         Just v -> v == value
                                         Nothing -> False
                               in (mx, f && flag)) (emptyMap, True) keysValues


main = quickCheck propMapInsertGetBatch

