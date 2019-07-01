{-# LANGUAGE TypeApplications, ScopedTypeVariables, LambdaCase #-}

module Control.Monad.Util (ifM, whenM, unlessM, andM, orM, findM) where

import Data.Coerce

import Data.Monoid
import Data.Bool

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c t f = c >>= bool f t
{-# INLINE ifM #-}

whenM :: Monad m => m Bool -> m () -> m ()
whenM c t = ifM c t (pure ())
{-# INLINE whenM #-}

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c f = ifM c (pure ()) f
{-# INLINE unlessM #-}

andM :: Monad m => m Bool -> m Bool -> m Bool
andM b1 b2 = b1 >>= \case
               True  -> b2
               False -> pure False
infixr 3 `andM`
{-# INLINE andM #-}

orM :: Monad m => m Bool -> m Bool -> m Bool
orM b1 b2 = b1 >>= \case
              True  -> pure True
              False -> b2
infixr 2 `orM`
{-# INLINE orM #-}

findM :: forall t m a. (Foldable t, Monad m) => (a -> m Bool) -> t a -> m (Maybe a)
findM p = coerce . foldMap @t @(Alt (MaybeT _) _) $ \x -> Alt $ lift (p x) >>= \case
                                                                  True  -> pure x
                                                                  False -> empty
{-# INLINABLE findM #-}
