{-# LANGUAGE AllowAmbiguousTypes #-}
-- |
-- Copyright: (c) 2022 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
--
-- Functions for effect handling, as well as effect stack manipulating.
module Sp.Internal.Handle
  ( -- * Interpret
    Interpret
  , interpret
  , interpret0
  , interpret1
  , interpret2
  , interpret3
  , interpretN
    -- * Interpose
  , Interpose
  , interpose
  , interpose0
  , interpose1
  , interpose2
  , interpose3
  , interposeN
    -- * Replace
  , Replace
  , replace
  , replace0
  , replace1
  , replace2
  , replace3
  , replaceN
    -- * Lift
  , Lift
  , lift
  , lift1
  , lift2
  , lift3
  , liftN
    -- * Lift Under
  , LiftNUnder
  , liftUnder1
  , lift1Under1
  , lift2Under1
  , lift3Under1
  , liftNUnder1
  , LiftUnderN
  , lift1Under2
  , lift1Under3
  , lift1UnderN
  , liftNUnderN
    -- * Subsume
  , Subsume
  , subsume1
  , subsume2
  , subsume3
  , subsumeN
    -- * Miscellaneous
  , inject
  , rearrange
  , rearrangeN
  ) where

import qualified Sp.Internal.Env   as Rec
import           Sp.Internal.Env   (KnownList, KnownSubset, Subset, Suffix, type (++), (:>))
import           Sp.Internal.Monad

--------------------------------------------------------------------------------
-- Interpret -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The family of /interpreting/ functions. Eliminate an effect from the top of the stack via a handler, and
-- optionally add some other effects (@es'@) that could be used in the handler. Adding these effects instead of
-- requiring them on the client side achieves effect encapsulation.
type Interpret es' = ∀ e es a. Handler e (es' ++ es) a -> Eff (e : es) a -> Eff (es' ++ es) a

-- | Interpret and add extra effects based on type inference. If this does not work consider using the more concrete
-- functions below.
interpret :: Suffix es es' => Handler e es' a -> Eff (e : es) a -> Eff es' a
interpret = handle \ih es -> Rec.cons ih $ Rec.suffix es
{-# INLINE interpret #-}

-- | Interpret and don't add extra effects.
interpret0 :: Interpret '[]
interpret0 = interpret
{-# INLINE interpret0 #-}

-- | Interpret and add 1 extra effect.
interpret1 :: Interpret '[e']
interpret1 = interpret
{-# INLINE interpret1 #-}

-- | Interpret and add 2 extra effects.
interpret2 :: Interpret '[e', e'']
interpret2 = interpret
{-# INLINE interpret2 #-}

-- | Interpret and add 3 extra effects.
interpret3 :: Interpret '[e', e'', e''']
interpret3 = interpret
{-# INLINE interpret3 #-}

-- | Interpret and add a list of extra effects specified explicitly via @TypeApplications@.
interpretN :: ∀ es'. KnownList es' => Interpret es'
interpretN = handle \ih es -> Rec.cons ih $ Rec.drop @es' es
{-# INLINE interpretN #-}

--------------------------------------------------------------------------------
-- Interpose -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The family of /interposing/ functions. Modify the implementation of an effect in the stack via a new handler, and
-- optionally add some other effects (@es'@) that could be used in said handler.
type Interpose es' = ∀ e es a. e :> es => Handler e (es' ++ es) a -> Eff es a -> Eff (es' ++ es) a

-- | Interpose and add extra effects based on type inference. If this does not work consider using the more concrete
-- functions below.
interpose :: (e :> es, Suffix es es') => Handler e es' a -> Eff es a -> Eff es' a
interpose = handle \ih es -> Rec.update ih $ Rec.suffix es
{-# INLINE interpose #-}

-- | Interpose and don't add extra effects.
interpose0 :: Interpose '[]
interpose0 = interpose
{-# INLINE interpose0 #-}

-- | Interpose and add 1 extra effect.
interpose1 :: Interpose '[e']
interpose1 = interpose
{-# INLINE interpose1 #-}

-- | Interpose and add 2 extra effects.
interpose2 :: Interpose '[e', e'']
interpose2 = interpose
{-# INLINE interpose2 #-}

-- | Interpose and add 3 extra effects.
interpose3 :: Interpose '[e', e'', e''']
interpose3 = interpose
{-# INLINE interpose3 #-}

-- | Interpose and add a list of extra effects specified explicitly via @TypeApplications@.
interposeN :: ∀ es'. KnownList es' => Interpose es'
interposeN = handle \ih es -> Rec.update ih $ Rec.drop @es' es
{-# INLINE interposeN #-}

--------------------------------------------------------------------------------
-- Replace ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The family of /interposing/ functions. Modify the implementation of an effect in the stack via a new handler, and
-- optionally add some other effects (@es'@) that could be used in said handler.
type Replace es' = ∀ e es a. e :> es => Handler e (es' ++ es) a -> Eff es a -> Eff (es' ++ es) a

-- | Replace and add extra effects based on type inference. If this does not work consider using the more concrete
-- functions below.
replace :: (e :> es, Suffix es es') => Handler e es' a -> Eff es a -> Eff es' a
replace = rehandle \es -> Rec.suffix es
{-# INLINE replace #-}

-- | Replace and don't add extra effects.
replace0 :: Replace '[]
replace0 = replace
{-# INLINE replace0 #-}

-- | Replace and add 1 extra effect.
replace1 :: Replace '[e']
replace1 = replace
{-# INLINE replace1 #-}

-- | Replace and add 2 extra effects.
replace2 :: Replace '[e', e'']
replace2 = replace
{-# INLINE replace2 #-}

-- | Replace and add 3 extra effects.
replace3 :: Replace '[e', e'', e''']
replace3 = replace
{-# INLINE replace3 #-}

-- | Replace and add a list of extra effects specified explicitly via @TypeApplications@.
replaceN :: ∀ es'. KnownList es' => Replace es'
replaceN = rehandle \es -> Rec.drop @es' es
{-# INLINE replaceN #-}

--------------------------------------------------------------------------------
-- Lift ------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The family of /lifting/ functions. They trivially lift a computation over some effects into a larger effect
-- context. It can be also seen as masking a set of effects from the inner computation.
type Lift es' = ∀ es a. Eff es a -> Eff (es' ++ es) a

-- | Lift over some effects based on type inference. If this does not work consider using the more concrete functions
-- below.
lift :: Suffix es es' => Eff es a -> Eff es' a
lift = alter Rec.suffix
{-# INLINE lift #-}

-- | Lift over 1 effect.
lift1 :: Lift '[e']
lift1 = lift
{-# INLINE lift1 #-}

-- | Lift over 2 effects.
lift2 :: Lift '[e', e'']
lift2 = lift
{-# INLINE lift2 #-}

-- | Lift over 3 effects.
lift3 :: Lift '[e', e'', e''']
lift3 = lift
{-# INLINE lift3 #-}

-- | Lift pver a list of effects supplied explicitly via @TypeApplications@.
liftN :: ∀ es'. KnownList es' => Lift es'
liftN = alter (Rec.drop @es')
{-# INLINE liftN #-}

--------------------------------------------------------------------------------
-- Lift Under ------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The family of /lifting-under-1/ functions. They lift over several effects /under/ one effect.
type LiftNUnder es' = ∀ e es a. Eff (e : es) a -> Eff (e : es' ++ es) a

-- | Lift over several effect under 1 effect, based on type inference. If this does not work consider using the more
-- concrete functions below.
liftUnder1 :: Suffix es es' => Eff (e : es) a -> Eff (e : es') a
liftUnder1 = alter \es -> Rec.cons (Rec.head es) $ Rec.suffix es
{-# INLINE liftUnder1 #-}

-- | Lift over 1 effect under 1 effect.
lift1Under1 :: LiftNUnder '[e']
lift1Under1 = liftUnder1
{-# INLINE lift1Under1 #-}

-- | Lift over 2 effects under 1 effect.
lift2Under1 :: LiftNUnder '[e', e'']
lift2Under1 = liftUnder1
{-# INLINE lift2Under1 #-}

-- | Lift over 3 effects under 1 effect.
lift3Under1 :: LiftNUnder '[e', e'', e''']
lift3Under1 = liftUnder1
{-# INLINE lift3Under1 #-}

-- | Lift over a list of effects under 1 effect. The list of effects is supplied explicitly via @TypeApplications@.
liftNUnder1 :: ∀ es'. KnownList es' => LiftNUnder es'
liftNUnder1 = alter \es -> Rec.cons (Rec.head es) $ Rec.drop @(_ : es') es
{-# INLINE liftNUnder1 #-}

-- | The family of /lifting-1-under/ functions. They lift over an effect /under several effects/. This family of
-- functions don't have inferred variants because they're hard to formulate.
type LiftUnderN es' = ∀ e es a. Eff (es' ++ es) a -> Eff (es' ++ e : es) a

-- | Lift over 1 effect under 2 effects.
lift1Under2 :: ∀ e' e''. LiftUnderN '[e', e'']
lift1Under2 = lift1UnderN @'[e', e'']
{-# INLINE lift1Under2 #-}

-- | Lift over 1 effect under 3 effects.
lift1Under3 :: ∀ e' e'' e'''. LiftUnderN '[e', e'', e''']
lift1Under3 = lift1UnderN @'[e', e'', e''']
{-# INLINE lift1Under3 #-}

-- | Lift over 1 effect under a list effects explicitly supplied via @TypeApplications@.
lift1UnderN :: ∀ es' e es a. KnownList es' => Eff (es' ++ es) a -> Eff (es' ++ e : es) a
lift1UnderN = alter \es -> Rec.concat (Rec.take @es' @(e : es) es) $ Rec.tail $ Rec.drop @es' @(e : es) es
{-# INLINE lift1UnderN #-}

-- | The most general /lifting-under/ function. Lifts over a list of effects under another list of effects. Both
-- lists are to supplied explicitly via @TypeApplications@.
liftNUnderN :: ∀ es'' es' es a. (KnownList es', KnownList es'') => Eff (es' ++ es) a -> Eff (es' ++ es'' ++ es) a
liftNUnderN = alter \es ->
  Rec.concat (Rec.take @es' @(es'' ++ es) es) $ Rec.drop @es'' @es $ Rec.drop @es' @(es'' ++ es) es
{-# INLINE liftNUnderN #-}

--------------------------------------------------------------------------------
-- Subsume ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The family of /subsumption/ functions. They trivially eliminate duplicate effects from the top of the stack. This
-- functions don't have inferred variants because they're hard to formulate.
type Subsume es' = ∀ es a. KnownSubset es' es => Eff (es' ++ es) a -> Eff es a

-- | Subsume 1 duplicate effect.
subsume1 :: ∀ e'. Subsume '[e']
subsume1 = subsumeN @'[e']
{-# INLINE subsume1 #-}

-- | Subsume 2 duplicate effects.
subsume2 :: ∀ e' e''. Subsume '[e', e'']
subsume2 = subsumeN @'[e', e'']
{-# INLINE subsume2 #-}

-- | Subsume 3 duplicate effects.
subsume3 :: ∀ e' e'' e'''. Subsume '[e', e'', e''']
subsume3 = subsumeN @'[e', e'', e''']
{-# INLINE subsume3 #-}

-- | Subsume a list duplicate effects explicitly supplied via @TypeApplications@.
subsumeN :: ∀ es'. KnownList es' => Subsume es'
subsumeN = alter \es -> Rec.concat (Rec.pick @es' es) es
{-# INLINE subsumeN #-}

--------------------------------------------------------------------------------
-- Miscellaneous ---------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Inject a small effect context with all elements known into a larger effect context.
inject :: KnownSubset es' es => Eff es' a -> Eff es a
inject = alter Rec.pick
{-# INLINE inject #-}

-- | Arbitrarily rearrange the known prefix of the effect context of a computation, as long as the polymorphic tail
-- is unchanged. This function is based on type inference, use 'inject' or 'rearrangeN' when it doesn't work properly.
rearrange :: Subset es es' => Eff es a -> Eff es' a
rearrange = alter Rec.extract
{-# INLINE rearrange #-}

-- | Like 'rearrange' but with the prefixes explicitly supplied via @TypeApplications@.
rearrangeN :: ∀ es' es'' es a. (KnownList es'', KnownSubset es' (es'' ++ es)) => Eff (es' ++ es) a -> Eff (es'' ++ es) a
rearrangeN = alter \es -> Rec.concat (Rec.pick @es' es) $ Rec.drop @es'' @es es
{-# INLINE rearrangeN #-}
