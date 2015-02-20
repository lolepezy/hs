{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances #-}
import Test.QuickCheck
import Test.HUnit
import Data.List (elem)
import Data.Set (toList, fromList)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class

type TTreeMap k v = TVar (TreeMapImpl k v)

data TreeMapImpl k v = Empty 
  | Leaf k v 
  | Node k v (TTreeMap k v) (TTreeMap k v)
  deriving (Eq)

emptySTMMap :: STM (TTreeMap k v)
emptySTMMap = newTVar Empty

insertImpl :: (Eq k, Ord k) => TreeMapImpl k v -> k -> v -> STM (TreeMapImpl k v)
insertImpl Empty key value = return $ Leaf key value
insertImpl (Leaf key value) key' value' 
  | key' < key = newTVar (Leaf key' value') >>= \left ->
                 newTVar Empty >>= \right ->
                 return $ Node key value left right
  | key' == key = return (Leaf key value')
  | otherwise =  newTVar Empty >>= \left ->
                 newTVar (Leaf key' value') >>= \right ->
                 return $ Node key value left right

insertImpl n@(Node key value left right) key' value' 
  | key' < key  = replaceSubTree left key' value' >>= \_ -> return $ n
  | key' == key = return $ Node key value' left right
  | otherwise   = replaceSubTree right key' value' >>= \_ -> return $ n
  where
    replaceSubTree tTree ke val = readTVar tTree >>= \t -> 
                   insertImpl t ke val >>= \newT -> 
                   writeTVar tTree newT

insert :: (Eq k, Ord k) => TTreeMap k v -> k -> v -> STM (TTreeMap k v)
insert stmMap k v = readTVar stmMap >>= \m -> 
                                 insertImpl m k v >>= \newM -> 
                                 writeTVar stmMap newM >>= \_ -> return stmMap


get :: (Eq k, Ord k) => TTreeMap k v -> k -> STM (Maybe v)
get stmMap k = readTVar stmMap >>= \m -> getImpl m k

getImpl :: (Eq k, Ord k) => TreeMapImpl k v -> k -> STM (Maybe v)

getImpl Empty _ = return Nothing
getImpl (Leaf key value) key' 
  | key == key' = return $ Just value 
  | otherwise   = return Nothing

getImpl (Node key value left right) key' 
  | key' < key  = readTVar left >>= \le -> getImpl le key'
  | key' == key = return $ Just value
  | key' > key  = readTVar right >>= \ri -> getImpl ri key'
  | otherwise   = return Nothing

{-
propMapInsertGetBatch :: [(String, Int)] -> Bool
propMapInsertGetBatch keysValues = snd $ foldr  
  (\(key, value) (mm, flag) -> let mx = insert mm key value 
                                   f = case get mx key of
                                         Just v -> v == value
                                         Nothing -> False
                               in (mx, f && flag)) (emptySTMMap, True) keysValues

-}

main = do
  x <- atomically $ emptySTMMap >>= \m -> 
                      insert m "1" "3" >>= \m -> 
                        get m "1"
  print x  -- quickCheck propMapInsertGetBatch

