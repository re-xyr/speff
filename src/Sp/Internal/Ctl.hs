module Sp.Internal.Ctl
  ( Marker
  , Ctl
  , prompt
  , yield
  , raise
  , promptState
  , runCtl
  , dynamicWind
  , mask
  , mask_
  , uninterruptibleMask
  , uninterruptibleMask_
  , interruptible
  ) where

import           Control.Exception      (MaskingState (MaskedInterruptible, MaskedUninterruptible, Unmasked),
                                         SomeException, getMaskingState)
import qualified Control.Exception      as Exception
import           Control.Monad          (ap, liftM, (<=<))
import           Control.Monad.Catch    (MonadCatch, MonadThrow)
import qualified Control.Monad.Catch    as Catch
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Atomics.Counter   (AtomicCounter, incrCounter, newCounter)
import           Data.IORef             (IORef, newIORef, readIORef, writeIORef)
import           Data.Kind              (Type)
import           Data.Type.Equality     (type (:~:) (Refl))
import           GHC.Exts               (maskAsyncExceptions#, maskUninterruptible#, unmaskAsyncExceptions#)
import           GHC.IO                 (IO (IO))
import           System.IO.Unsafe       (unsafePerformIO)
import           Unsafe.Coerce          (unsafeCoerce)

uniqueSource :: AtomicCounter
uniqueSource = unsafePerformIO (newCounter 0)
{-# NOINLINE uniqueSource #-}

freshMarker :: ∀ a. Ctl (Marker a)
freshMarker = liftIO $ Marker <$> incrCounter 1 uniqueSource

type role Marker representational
newtype Marker (a :: Type) = Marker Int

-- | Check the equality of two markers, and if so provide a proof that the type parameters are equal. This does not
-- warrant a @TestEquality@ instance because it requires decidable equality over the type parameters.
eqMarker :: Marker a -> Marker b -> Maybe (a :~: b)
eqMarker (Marker l) (Marker r) =
    if l == r then Just (unsafeCoerce Refl) else Nothing

-- We don't force the value because that makes the semantics nonstandard
-- Plus lazy semantics seems to make benchmarks faster
type role Result representational
data Result (a :: Type)
  = Pure a
  | ∀ (r :: Type). Raise !(Marker r) (Ctl r)
  | ∀ (r :: Type) (b :: Type). Yield !(Marker r) ((Ctl b -> Ctl r) -> Ctl r) (Ctl b -> Ctl a)

extend :: (Ctl a -> Ctl a) -> Result a -> Result a
extend f = \case
  Pure a              -> Pure a
  Raise mark r        -> Raise mark r
  Yield mark ctl cont -> Yield mark ctl (f . cont)

type role Ctl representational
newtype Ctl (a :: Type) = Ctl { unCtl :: IO (Result a) }

instance Functor Ctl where
  fmap = liftM

instance Applicative Ctl where
  pure = Ctl . pure . Pure
  (<*>) = ap

instance Monad Ctl where
  (Ctl x) >>= f = Ctl $ x >>= \case
    Pure a              -> unCtl (f a)
    Raise mark r        -> pure $ Raise mark r
    Yield mark ctl cont -> pure $ Yield mark ctl (f `compose` cont)

compose :: (b -> Ctl c) -> (a -> Ctl b) -> a -> Ctl c
compose = (<=<)
{-# NOINLINE compose #-}

liftMap :: (IO (Result a) -> IO (Result a)) -> Ctl a -> Ctl a
liftMap f (Ctl m) = Ctl $ extend (liftMap f) <$> f m

prompt :: ∀ a. (Marker a -> Ctl a) -> Ctl a
prompt f = do
  mark <- freshMarker @a
  promptWith mark (f mark)

promptWith :: Marker a -> Ctl a -> Ctl a
promptWith !mark m = Ctl $ unCtl m >>= \case
  Pure a -> pure $ Pure a
  Raise mark' r -> case eqMarker mark mark' of
    Just Refl -> unCtl r
    Nothing   -> pure $ Raise mark' r
  Yield mark' ctl cont -> case eqMarker mark mark' of
    Just Refl -> unCtl $ ctl (promptWith mark . cont)
    Nothing   -> pure $ Yield mark' ctl (promptWith mark . cont)

-- yielding is not strict in f
yield :: Marker r -> ((Ctl a -> Ctl r) -> Ctl r) -> Ctl a
yield !mark f = Ctl $ pure $ Yield mark f id

-- raising is not strict in r
raise :: Marker r -> Ctl r -> Ctl a
raise !mark r = Ctl $ pure $ Raise mark r

promptState :: ∀ s r. s -> (IORef s -> Ctl r) -> Ctl r
promptState x0 f = do
  ref <- liftIO (newIORef x0)
  promptStateWith ref (f ref)

promptStateWith :: IORef s -> Ctl r -> Ctl r
promptStateWith !ref (Ctl m) = Ctl $ m >>= \case
  Pure x -> pure $ Pure x
  Raise mark x -> pure $ Raise mark x
  Yield mark ctl cont -> do
    s0 <- liftIO (readIORef ref)
    pure $ Yield mark ctl \x -> do
      liftIO (writeIORef ref s0)
      promptStateWith ref (cont x)

runCtl :: Ctl a -> IO a
runCtl (Ctl m) = m >>= \case
  Pure a   -> pure a
  Raise {} -> error "Sp.Ctl: Unhandled raise operation. Forgot to pair it with a prompt?"
  Yield {} -> error "Sp.Ctl: Unhandled yield operation. Forgot to pair it with a prompt?"

instance MonadIO Ctl where
  liftIO = Ctl . fmap Pure

instance MonadThrow Ctl where
  throwM = Ctl . Exception.throwIO

instance MonadCatch Ctl where
  catch m h = liftMap (Exception.handle (unCtl . h)) m

dynamicWind :: Ctl () -> Ctl () -> Ctl a -> Ctl a
dynamicWind before after (Ctl action) = do
  res <- before >> Ctl do
    res <- Exception.try @SomeException action
    pure $ Pure res
  after >> Ctl case res of
    Left se -> Exception.throwIO se
    Right y -> pure $ extend (dynamicWind before after) y

block, unblock, blockUninterruptible :: Ctl a -> Ctl a
block = liftMap \(IO m) -> IO $ maskAsyncExceptions# m
unblock = liftMap \(IO m) -> IO $ unmaskAsyncExceptions# m
blockUninterruptible = liftMap \(IO m) -> IO $ maskUninterruptible# m

mask, uninterruptibleMask :: ((forall x. Ctl x -> Ctl x) -> Ctl a) -> Ctl a
mask io = liftIO getMaskingState >>= \case
  Unmasked              -> block $ io unblock
  MaskedInterruptible   -> io block
  MaskedUninterruptible -> io blockUninterruptible
uninterruptibleMask io = liftIO getMaskingState >>= \case
  Unmasked              -> blockUninterruptible $ io unblock
  MaskedInterruptible   -> blockUninterruptible $ io block
  MaskedUninterruptible -> io blockUninterruptible

mask_, uninterruptibleMask_ :: Ctl a -> Ctl a
mask_ io = mask (\_ -> io)
uninterruptibleMask_ io = uninterruptibleMask (\_ -> io)

interruptible :: Ctl a -> Ctl a
interruptible io = liftIO getMaskingState >>= \case
  Unmasked              -> io
  MaskedInterruptible   -> unblock io
  MaskedUninterruptible -> io
