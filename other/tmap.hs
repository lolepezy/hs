
import Test.QuickCheck
import Test.QuickCheck.Monadic as QCM
import Data.List (elem)
import Data.Set (toList, fromList)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
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
  let m = emptySTMMap
  qq <- QCM.run $ do 
    -- so some random transformations of arguments
    a0 <- async $ insertAndGetAll m $ filter ((>=0) . fst) keysAndValues
    a1 <- async $ insertAndGetAll m $ filter ((<0) . fst) keysAndValues
    a2 <- async $ insertAndGetAll m $ map (\(k,v) -> (2*k, v)) keysAndValues
    a3 <- async $ insertAndGetAll m $ keysAndValues
    a4 <- async $ insertAndGetAll m $ keysAndValues
    r0 <- wait a0
    r1 <- wait a1
    r2 <- wait a2
    r3 <- wait a3
    r4 <- wait a4
    return $ r0 && r1 && r2 && r3 && r4
  assert $ qq
  where
    insertAndGetAll :: STM (TTreeMap Int String) -> [(Int, String)] -> IO Bool
    insertAndGetAll mm kvs = atomically $ do
      mz <- mm
      (m, e) <- foldM insertAndGet (mz, True) kvs
      return $ e

    insertAndGet :: (TTreeMap Int String, Bool) -> (Int, String) -> STM (TTreeMap Int String, Bool)
    insertAndGet (m, equals) (k, v) = do
      m' <- insert m k v
      v' <- get m' k
      return $ (m', equals && case v' of
                                Just q -> q == v
                                Nothing -> False)


main = do
  quickCheck prop_insertAndGet

