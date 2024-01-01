{-# LANGUAGE AllowAmbiguousTypes #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
--
-- This module defines an immutable extensible record type, similar to @vinyl@ and @data-diverse@. However this
-- implementation focuses on fast reads, hence has very different performance characteristics from other libraries:
--
-- * Lookup: Amortized /O/(1).
-- * Update: /O/(/n/).
-- * Shrink: /O/(1).
-- * Append: /O/(/n/).
module Sp.Internal.Env
  ( Rec
  , length
    -- * Construction
  , empty
  , cons
  , pad
  , type (++)
  , concat
    -- * Deconstruction
  , head
  , tail
  , type KnownList
  , drop
  , take
    -- * Retrieval & Update
  , type (:>)
  , index
  , update
    -- * Subset operations
  , type Suffix
  , suffix
  , type KnownSubset
  , pick
  , type AllMembers
  , type Subset
  , extract
  ) where

import           Data.Kind       (Constraint, Type)
import           GHC.Exts        (Any)
import           GHC.TypeLits    (ErrorMessage (ShowType, Text, (:<>:)), TypeError)
import           Prelude         hiding (concat, drop, head, length, tail, take)
import qualified Sp.Internal.Vec as Vec
import           Sp.Internal.Vec (ConcatPhase (..), DropPhase (..), Vec)
import           Unsafe.Coerce   (unsafeCoerce)

-- | Extensible record type supporting efficient /O/(1) reads.
type role Rec representational nominal
newtype Rec (f :: k -> Type) (es :: [k]) = Rec (Vec Any)

-- | Get the length of the record.
length :: Rec f es -> Int
length (Rec vec) = Vec.length vec
{-# INLINE length #-}

unreifiable :: String -> String -> a
unreifiable clsName comp = error $
  "Sp.Internal.Env: Attempting to access " <> comp <> " without a reflected value. This is perhaps because you are \
  \trying to define an instance for the '" <> clsName <> "' typeclass, which you should not be doing whatsoever. If \
  \that seems unlikely, please report this as a bug."

--------------------------------------------------------------------------------
-- Construction ----------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Create an empty record. /O/(1).
empty :: Rec f '[]
empty = Rec Vec.empty
{-# INLINE empty #-}

-- | Prepend one entry to the record. /O/(/n/).
cons :: f e -> Rec f es -> Rec f (e ': es)
cons x (Rec vec) = Rec $ Vec.cons (toAny x) vec
{-# INLINE cons #-}

-- | Prepend one null entry to the record. This entry can be normally evaluated (different from 'undefined'), but any
-- attempt to use it will cause a runtime error. /O/(/n/).
pad :: Rec f es -> Rec f (e ': es)
pad (Rec vec) = Rec $ Vec.pad vec
{-# INLINE pad #-}

-- | Type level list concatenation.
type family (xs :: [k]) ++ (ys :: [k]) where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)
infixr 5 ++

-- | Concatenate two records. /O/(/m/ + /n/).
concat :: Rec f es -> Rec f es' -> Rec f (es ++ es')
concat (Rec vec) (Rec vec') = Rec $ Vec.concat vec vec'
{-# INLINE concat #-}

--------------------------------------------------------------------------------
-- Deconstruction --------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Get the head of the record. /O/(1).
head :: Rec f (e ': es) -> f e
head (Rec vec) = fromAny $ Vec.head vec
{-# INLINE head #-}

-- | Slice off one entry from the top of the record. /O/(1).
tail :: Rec f (e ': es) -> Rec f es
tail (Rec vec) = Rec $ Vec.tail vec
{-# INLINE tail #-}

-- | The list @es@ list is concrete, i.e. is of the form @'[a1, a2, ..., an]@ therefore having a known length.
class KnownList (es :: [k]) where
  -- | Get the length of the list.
  reifyLen :: Int
  reifyLen = unreifiable "KnownList" "the length of a type-level list"

instance KnownList '[] where
  reifyLen = 0

instance KnownList es => KnownList (e ': es) where
  reifyLen = 1 + reifyLen @_ @es

-- | Slice off several entries from the top of the record. Amortized /O/(1).
drop :: ∀ es es' f. KnownList es => Rec f (es ++ es') -> Rec f es'
drop (Rec vec) = Rec $ Vec.drop (reifyLen @_ @es) vec
{-# INLINE drop #-}

-- | Take elements from the top of the record. /O/(/m/).
take :: ∀ es es' f. KnownList es => Rec f (es ++ es') -> Rec f es
take (Rec vec) = Rec $ Vec.take (reifyLen @_ @es) vec
{-# INLINE take #-}

--------------------------------------------------------------------------------
-- Retrieval & Update ----------------------------------------------------------
--------------------------------------------------------------------------------

-- | The element @e@ is present in the list @es@.
class (e :: k) :> (es :: [k]) where
  -- | Get the index of the element.
  reifyIndex :: Int
  reifyIndex = unreifiable "Elem" "an element of a type-level list"
infix 0 :>

instance {-# OVERLAPPING #-} e :> e : es where
  reifyIndex = 0

instance e :> es => e :> e' : es where
  reifyIndex = 1 + reifyIndex @_ @e @es

type ElemNotFound e = 'Text "The element '" ':<>: 'ShowType e ':<>: 'Text "' is not present in the constraint"

instance TypeError (ElemNotFound e) => e :> '[] where
  reifyIndex = error "Sp.Env: Attempting to refer to a nonexistent member. Please report this as a bug."

-- | Get an element in the record. Amortized /O/(1).
index :: ∀ e es f. e :> es => Rec f es -> f e
index (Rec vec) = fromAny $ Vec.index (reifyIndex @_ @e @es) vec
{-# INLINE index #-}

-- | Update an entry in the record. /O/(/n/).
update :: ∀ e es f. e :> es => f e -> Rec f es -> Rec f es
update x (Rec vec) = Rec $ Vec.update (reifyIndex @_ @e @es) (toAny x) vec
{-# INLINE update #-}

--------------------------------------------------------------------------------
-- Subset Operations -----------------------------------------------------------
--------------------------------------------------------------------------------

-- | @es@ is a suffix of @es'@. This works even if both has a shared polymorphic tail, e.g. this typeclass recognizes
-- @2 : 3 : es@ is a suffix of @0 : 1 : 2 : 3 : es@.
class Suffix (es :: [k]) (es' :: [k]) where
  reifyPrefix :: Int
  reifyPrefix = unreifiable "Subset" "a prefix of a type-level list"

instance Suffix es es where
  reifyPrefix = 0

-- | This is morally coherent because if it actually turned out that @es ~ e : es'@, the search will simply fail
-- instead of producing a different instance.
instance {-# INCOHERENT #-} Suffix es es' => Suffix es (e : es') where
  reifyPrefix = 1 + reifyPrefix @_ @es @es'

-- | Get a suffix of the record. Amortized /O/(1).
suffix :: ∀ es es' f. Suffix es es' => Rec f es' -> Rec f es
suffix (Rec vec) = Rec $ Vec.drop (reifyPrefix @_ @es @es') vec
{-# INLINE suffix #-}

-- | Shorthand for @(e1 ':>' es, e2 :> es, e3 :> es, ...)@.
type family AllMembers (es :: [k]) (es' :: [k]) :: Constraint where
  AllMembers '[] _ = ()
  AllMembers (e : es) es' = (e :> es', AllMembers es es')

-- | @es@ is a known subset of @es'@, i.e. all members of @es@ have a statically known index in @es'@.
class (KnownList es, AllMembers es es') => KnownSubset (es :: [k]) (es' :: [k]) where
  -- | Get a list of indices of the elements.
  reifyIndices :: [Int]
  reifyIndices = unreifiable "KnownSubset" "multiple elements of a type-level list"

instance KnownSubset '[] es where
  reifyIndices = []

instance (KnownSubset es es', e :> es') => KnownSubset (e ': es) es' where
  reifyIndices = reifyIndex @_ @e @es' : reifyIndices @_ @es @es'

-- | Get a known subset of the record. Amortized /O/(/m/).
pick :: ∀ es es' f. KnownSubset es es' => Rec f es' -> Rec f es
pick (Rec vec) = Rec $ Vec.pick (reifyLen @_ @es) (reifyIndices @_ @es @es') vec
{-# INLINE pick #-}

-- | @es@ is a subset of @es'@. This works on both known lists and lists with polymorphic tails. E.g. all of the
-- following work:
--
-- @
-- Subset '[3, 1] '[1, 2, 3]
-- Subset '[3, 1] (1 : 2 : 3 : es)
-- Subset (3 : 1 : es) (1 : 2 : 3 : es)
-- @
class Subset (es :: [k]) (es' :: [k]) where
  reifyExtraction :: ConcatPhase
  reifyExtraction = unreifiable "Subset" "a subsequence of a type-level list"

instance Subset '[] es' where
  reifyExtraction = IdOp EmptyOp

instance (e :> es', Subset es es') => Subset (e : es) es' where
  reifyExtraction = case reifyExtraction @_ @es @es' of
    IdOp ro          -> ConcatOp 1 [reifyIndex @_ @e @es'] ro
    ConcatOp n xs ro -> ConcatOp (1 + n) (reifyIndex @_ @e @es' : xs) ro

instance {-# INCOHERENT #-} Suffix es es' => Subset es es' where
  reifyExtraction = IdOp (DropOp $ reifyPrefix @_ @es @es')

-- | Extract a subset out of the record. /O/(/n/).
extract :: ∀ es es' f. Subset es es' => Rec f es' -> Rec f es
extract (Rec vec) = Rec $ Vec.extract (reifyExtraction @_ @es @es') vec
{-# INLINE extract #-}

--------------------------------------------------------------------------------
-- Any -------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Coerce any boxed value into 'GHC.Exts.Any'.
toAny :: a -> Any
toAny = unsafeCoerce

-- | Coerce an 'GHC.Exts.Any' value to a certain type. This is generally unsafe and it is your responsibility to ensure
-- that the type you're coercing into is the original type that the 'GHC.Exts.Any' value is coerced from.
fromAny :: Any -> a
fromAny = unsafeCoerce
