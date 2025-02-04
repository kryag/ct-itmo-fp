{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module HW6.T1
  ( BucketsArray
  , CHT (..)

  , newCHT
  , getCHT
  , putCHT
  , sizeCHT

  , initCapacity
  , loadFactor
  ) where

import Control.Concurrent.Classy (STM, MonadConc (atomically),
  MonadSTM (newTVar, readTVar, writeTVar), readTVarConc)
import Control.Concurrent.Classy.STM (TArray, TVar)
import Data.Array.Base (getNumElements, getElems, newArray, readArray, writeArray)
import Data.Hashable (Hashable (hash))
import Control.Monad (when)
import Data.Foldable (for_)

-- | Initial capacity (number of buckets) for a new hash table.
initCapacity :: Int
initCapacity = 16

-- | The load factor threshold. Resizing occurs when size >= capacity * loadFactor.
loadFactor :: Double
loadFactor = 0.75

-- | Each bucket is a simple list of key-value pairs.
type Bucket k v = [(k, v)]
-- | 'BucketsArray' is a 'TArray' from index 'Int' to a 'Bucket'.
type BucketsArray stm k v = TArray stm Int (Bucket k v)

-- | The Concurrent Hash Table type, holding a 'TVar' of the buckets array
-- and a 'TVar' of the current element count. All operations are performed in STM.
data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  , chtSize    :: TVar stm Int
  }

-- | Picks the correct bucket given a key, returning the bucket and its index.
pickBucket
  :: (MonadSTM stm, Hashable k)
  => k
  -> BucketsArray stm k v
  -> stm (Bucket k v, Int)
pickBucket key buckets = do
  capacity <- getNumElements buckets
  let idx = hash key `mod` capacity
  bucket <- readArray buckets idx
  return (bucket, idx)

-- | Creates a new 'CHT' with 'initCapacity' and size set to 0.
newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT = atomically $ do
  storage <- newArray (0, initCapacity - 1) []
  buckets <- newTVar storage
  size <- newTVar 0
  return (CHT buckets size)

-- | Retrieves the value associated with a key from the concurrent hash table.
-- Returns 'Nothing' if the key is not present.
getCHT
  :: ( MonadConc m
     , Hashable k
     )
  => k
  -> CHT (STM m) k v
  -> m (Maybe v)
getCHT key table = atomically $ do
  buckets <- readTVar $ chtBuckets table
  (bucket, _) <- pickBucket key buckets
  return $ lookup key bucket

-- | Inserts a key-value pair into the hash table.
-- If the key already exists, nothing is changed.
-- If the size exceeds capacity * loadFactor, the table is resized.
putCHT
  :: ( MonadConc m
     , Hashable k
     )
  => k
  -> v
  -> CHT (STM m) k v
  -> m ()
putCHT key value table = atomically $ do
  buckets <- readTVar $ chtBuckets table
  (bucket, idx) <- pickBucket key buckets
  case lookup key bucket of
    Nothing -> do
      writeArray buckets idx ((key, value) : bucket)
      size <- readTVar (chtSize table)
      writeTVar (chtSize table) (size + 1)
      capacity <- getNumElements buckets
      when (fromIntegral (size + 1) >= fromIntegral capacity * loadFactor) $ resizeTable table
    Just _  -> return ()

-- | Resizes the table by doubling its capacity and re-inserting elements.
resizeTable :: (MonadSTM stm, Hashable k) => CHT stm k v -> stm ()
resizeTable table = do
  buckets <- readTVar $ chtBuckets table
  capacity <- getNumElements buckets
  let newCapacity = 2 * capacity
  newBuckets <- newArray (0, newCapacity - 1) []
  elems <- getElems buckets
  for_ elems $ \bucket ->
    for_ bucket $ \(k, v) -> do
      (newBucket, newIndex) <- pickBucket k newBuckets
      writeArray newBuckets newIndex $ (k, v) : newBucket
  writeTVar (chtBuckets table) newBuckets

-- | Returns the current size of the hash table.
sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT = readTVarConc . chtSize
