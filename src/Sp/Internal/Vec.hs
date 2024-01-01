-- |
-- Copyright: (c) 2022 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
module Sp.Internal.Vec
  ( Vec
  , length
  , empty
  , cons
  , pad
  , concat
  , head
  , tail
  , drop
  , take
  , index
  , update
  , pick
  , extract
  , DropPhase (..)
  , ConcatPhase (..)
  ) where

import           Control.Monad.ST     (ST)
import           Data.Foldable        (for_)
import           Data.Kind            (Type)
import           Data.Primitive.Array (Array, MutableArray, copyArray, emptyArray, indexArray, newArray, runArray,
                                       writeArray)
import           Prelude              hiding (concat, drop, head, length, tail, take)

-- | A vector (i.e. array slice) type. One special feature of this type is that it supports efficient (/O/(1))
-- prepending of "inaccessible" elements.
data Vec (a :: Type) = Vec !Int !Int !(Array a)

nil :: a
nil = error "Sp.Internal.Vec: uninitialized element"

copyArrayOffset :: MutableArray s a -> Int -> Array a -> Int -> Int -> ST s ()
copyArrayOffset marr ix arr off len = copyArray marr (ix - min 0 off) arr (max 0 off) len

indexArrayOffset :: Array a -> Int -> a
indexArrayOffset arr ix
  | ix < 0 = nil
  | otherwise = indexArray arr ix

-- | Get the length of the vector. \( O(1) \).
length :: Vec a -> Int
length (Vec _ len _) = len

-- | Create an empty record. \( O(1) \).
empty :: Vec a
empty = Vec 0 0 $ emptyArray

-- | Prepend one entry to the vector. \( O(n) \).
cons :: a -> Vec a -> Vec a
cons x (Vec off len arr) = Vec 0 (len + 1) $ runArray do
  marr <- newArray (len + 1) x
  copyArrayOffset marr 1 arr off len
  pure marr

-- | Prepend an inaccessible entry to the vector. \( O(n) \).
pad :: Vec a -> Vec a
pad (Vec off len arr) = Vec (off - 1) (len + 1) arr

-- | Concatenate two vectors. \( O(m+n) \).
concat :: Vec a -> Vec a -> Vec a
concat (Vec off len arr) (Vec off' len' arr') = Vec 0 (len + len') $ runArray do
  marr <- newArray (len + len') nil
  copyArrayOffset marr 0 arr off len
  copyArrayOffset marr len arr' off' len'
  pure marr

-- | Get the head of the vector. \( O(1) \).
head :: Vec a -> a
head (Vec off _ arr) = indexArray arr off

-- | Slice off one entry from the head of the vector. \( O(1) \).
tail :: Vec a -> Vec a
tail (Vec off len arr) = Vec (off + 1) (len - 1) arr

-- | Slice off several entries from the head of the vector. \( O(1) \).
drop :: Int -> Vec a -> Vec a
drop len' (Vec off len arr) = Vec (off + len') (len - len') arr

-- | Take elements from the head of the vector. \( O(m) \).
take :: Int -> Vec a -> Vec a
take len (Vec off _ arr) = Vec 0 len $ runArray do
  marr <- newArray len nil
  copyArrayOffset marr 0 arr off (off + len)
  pure marr

-- | Get an element in the vector. \( O(1) \).
index :: Int -> Vec a -> a
index ix (Vec off _ arr) = indexArray arr (off + ix)

-- | Update an entry in the record. \( O(n) \).
update :: Int -> a -> Vec a -> Vec a
update ix x (Vec off len arr) = Vec 0 len $ runArray do
  marr <- newArray len nil
  copyArrayOffset marr 0 arr off (off + len)
  writeArray marr ix x
  pure marr

-- | Get a known subset of the vector. \( O(m) \).
pick :: Int -> [Int] -> Vec a -> Vec a
pick len ixs (Vec off _ arr) = Vec 0 len $ runArray do
  marr <- newArray len nil
  for_ (zip [0..] ixs) \(newIx, oldIx) ->
    writeArray marr newIx $ indexArrayOffset arr (off + oldIx)
  pure marr

-- | A drop operation: either empty the entire vector or drop a certain number of entries from the head.
data DropPhase = EmptyOp | DropOp !Int

-- | A drop-and-concat operation: perform a drop operation, then optionally concat back some entries from the original
-- vector.
data ConcatPhase = IdOp DropPhase | ConcatOp !Int [Int] DropPhase

-- | Extract a subset out of the vector. \( O(n) \).
extract :: ConcatPhase -> Vec a -> Vec a
extract ext (Vec off len arr) = case ext of
  IdOp ro -> case ro of
    EmptyOp        -> Vec 0 0 emptyArray
    DropOp dropped -> Vec (off + dropped) (len - dropped) arr
  ConcatOp added addIxs ro -> case ro of
    EmptyOp  -> Vec 0 added $ runArray do
      marr <- newArray added nil
      for_ (zip [0..] $ addIxs) \(newIx, oldIx) ->
        writeArray marr newIx $ indexArrayOffset arr (off + oldIx)
      pure marr
    DropOp dropped -> Vec 0 (len - dropped + added) $ runArray do
      marr <- newArray (len - dropped + added) nil
      for_ (zip [0..] $ addIxs) \(newIx, oldIx) ->
        writeArray marr newIx $ indexArrayOffset arr (off + oldIx)
      for_ [0 .. len - dropped - 1] \ix ->
        writeArray marr (added + ix) $ indexArrayOffset arr (off + dropped + ix)
      pure marr
