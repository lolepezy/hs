
import Test.QuickCheck
import Test.QuickCheck.Monadic as QCM
import Data.List (elem)
import Data.Set (toList, fromList)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class

type TTreeMap k v = TVar (TreeMapImpl k v)

data TreeMapImpl k v = Empty 
  | Leaf !k !v 
  | Node !k !v !(TTreeMap k v) !(TTreeMap k v)
  deriving (Eq)

emptySTMMap :: STM (TTreeMap k v)
emptySTMMap = newTVar Empty

insertImpl :: (Eq k, Ord k) => TreeMapImpl k v -> k -> v -> STM (TreeMapImpl k v)
insertImpl Empty key value = return $ Leaf key value
insertImpl (Leaf key value) key' value' 
  | key' < key  = newTVar (Leaf key' value') >>= \left ->
                  newTVar Empty >>= \right ->
                  return $ Node key value left right
  | key' == key = return (Leaf key value')
  | otherwise   = newTVar Empty >>= \left ->
                  newTVar (Leaf key' value') >>= \right ->
                  return $ Node key value left right

insertImpl n@(Node key value left right) key' value' 
  | key' < key  = replaceSubTree left key' value' >> return n
  | key' == key = return $ Node key value' left right
  | otherwise   = replaceSubTree right key' value' >> return n
  where
    replaceSubTree tTree ke val = readTVar tTree >>= \t -> 
                   insertImpl t ke val >>= \newT -> 
                   writeTVar tTree newT

insert :: (Eq k, Ord k) => TTreeMap k v -> k -> v -> STM (TTreeMap k v)
insert stmMap k v = readTVar stmMap >>= \m -> 
                    insertImpl m k v >>= \newM -> 
                    writeTVar stmMap newM >> return stmMap


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
select blabla::([], [[]]) from :: x 
-}

prop_insertAndGet :: Property
prop_insertAndGet = QCM.monadicIO $ do 
  keysAndValues <- pick arbitrary
  qq <- QCM.run $ insertAndGet emptySTMMap keysAndValues
  assert $ qq
  where
    insertAndGet :: STM (TTreeMap Int String) -> [(Int, String)] -> IO Bool
    insertAndGet mm kvs = atomically $ do
      mz <- mm
      (m, e) <- foldM ig (mz, True) kvs
      return $ e

    ig :: (TTreeMap Int String, Bool) -> (Int, String) -> STM (TTreeMap Int String, Bool)
    ig (m, equals) (k, v) = do
      m' <- insert m k v
      v' <- get m' k
      return $ (m', equals && case v' of
                                Just q -> q == v
                                Nothing -> False)


main = do
  quickCheck prop_insertAndGet

