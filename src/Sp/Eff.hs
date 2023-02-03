module Sp.Eff
  ( -- * Basic types and operations
    Effect
  , Eff
  , IOE
  , (:>)
    -- ** Performing effects
  , send
    -- ** Unwrapping
  , runIOE
  , runEff
    -- * Effect handling
  , Handling
  , Handler
    -- ** Providing handlers
  , Suffix
  , type (++)
    -- *** Interpret
  , Interpret
  , interpret
  , interpret0
  , interpret1
  , interpret2
  , interpret3
  , interpretN
    -- *** Interpose
  , Interpose
  , interpose
  , interpose0
  , interpose1
  , interpose2
  , interpose3
  , interposeN
    -- ** Combinators to use in handlers
  , embed
  , withUnembed
  , abort
  , control
  , Localized
    -- * Trivial transformations
  , KnownList
  , KnownSubset
    -- ** Lift
  , Lift
  , lift
  , lift1
  , lift2
  , lift3
  , liftN
    -- ** Lift Under
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
    -- ** Subsume
  , Subsume
  , subsume1
  , subsume2
  , subsume3
  , subsumeN
    -- ** Miscellaneous
  , Subset
  , inject
  , rearrange
  , rearrangeN
  ) where

import           Sp.Internal.Env    (KnownList, KnownSubset, Subset, Suffix, type (++), (:>))
import           Sp.Internal.Handle
import           Sp.Internal.Monad
