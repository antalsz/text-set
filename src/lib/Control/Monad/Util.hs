{-# LANGUAGE TypeApplications, ScopedTypeVariables, LambdaCase #-}

module Control.Monad.Util (ifM, whenM, unlessM, findM) where

import Data.Coerce

import Data.Monoid
import Data.Bool

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c t f = c >>= bool f t

whenM :: Monad m => m Bool -> m () -> m ()
whenM c t = ifM c t (pure ())

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c f = ifM c (pure ()) f

findM :: forall t m a. (Foldable t, Monad m) => (a -> m Bool) -> t a -> m (Maybe a)
findM p = coerce . foldMap @t @(Alt (MaybeT _) _) $ \x -> Alt $ lift (p x) >>= \case
                                                                  True  -> pure x
                                                                  False -> empty
