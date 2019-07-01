{-|
Module      : Text.DAFSA.Graph
Description : Build a DAFSA, as per "Incremental Construction Minimal Acyclic Finite-State Automata"
Copyright   : Copyright © 2019 Antal Spector-Zabusky
License     : MIT
Maintainer  : Antal Spector-Zabusky <antal.b.sz@gmail.com>

This module implements Algorithm 1 from the paper "Incremental Construction of
Minimal Acyclic Finite-State Automata", by Jan Daciuk, Stoyan Mihov, Bruce
W. Watson, and Richard E. Watson.  Published in 2000 in /Computational
Linguistics/ 26(1), pp.3-16.  Available online at
<https://www.aclweb.org/anthology/J00-1002>.
-}

{-# LANGUAGE TypeApplications, ScopedTypeVariables,
             BangPatterns, TupleSections, LambdaCase, RecordWildCards #-}

module Text.DAFSA.Graph (
  -- * DFA states
  DFAState(..),
  equiv,
  walkPrefix,
  dfaInsert,
  wordDFA,

  -- * Constructing DAFSAs
  fromAscST,
  -- ** Subsidiary function
  replaceOrRegister,

  -- * 'HashTable's
  insertNewOrElse
) where

import Data.Functor
import Data.Foldable

import Data.Hashable
import qualified Data.HashTable.ST.Cuckoo as Cuckoo
import qualified Data.HashTable.Class     as H

import Control.Monad.Util
import Control.Monad.ST.Strict
import Data.STRef.Strict

import Text.DAFSA.ID

data DFAState s = DFAState { stateId   :: !ID
                           , lastChild :: !(STRef s Char)
                           , accept    :: !(STRef s Bool)
                           , children  :: !(Cuckoo.HashTable s Char (DFAState s)) }

-- Module-local
equiv_children :: Cuckoo.HashTable s Char (DFAState s) -> Cuckoo.HashTable s Char (DFAState s) -> ST s Bool
equiv_children kids1 kids2 =
  let this `subset` that =
        H.foldM (\b (c,q) -> pure b `andM` any ((stateId q ==) . stateId) <$> H.lookup that c) True this
      {-# INLINE subset #-}
  in (kids1 `subset` kids2) `andM` (kids2 `subset` kids1)
{-# INLINE equiv_children #-}

equiv :: DFAState s -> DFAState s -> ST s Bool
equiv state1 state2 =
  (==) <$> readSTRef (accept state1) <*> readSTRef (accept state2) `andM`
  children state1 `equiv_children` children state2

walkPrefix :: DFAState s -> String -> ST s (DFAState s, String)
walkPrefix !state             []       = pure (state, [])
walkPrefix state@DFAState{..} w@(c:cs) = H.lookup children c >>= \case
                                           Just state' -> walkPrefix state' cs
                                           Nothing     -> pure (state, w)

insertNewOrElse :: (H.HashTable h, Eq k, Hashable k) => h s k v -> k -> ST s v -> (v -> ST s ()) -> ST s ()
insertNewOrElse h k whenAbsent whenPresent =
  H.mutateST h k $ \case
    Just v  -> (Just v,)    <$> whenPresent v
    Nothing -> (,()) . Just <$> whenAbsent
{-# INLINABLE insertNewOrElse #-}

dfaInsert :: IDAllocator s -> DFAState s -> String -> ST s ()
dfaInsert idAllocator = go where
  go DFAState{..} []     = writeSTRef accept True
  go DFAState{..} (c:cs) = insertNewOrElse children c
                             (modifySTRef' lastChild (max c) *> wordDFA idAllocator cs)
                             (\state' -> dfaInsert idAllocator state' cs)
{-# INLINABLE dfaInsert #-}

wordDFA :: IDAllocator s -> [Char] -> ST s (DFAState s)
wordDFA idAllocator = go where
  go [] = do
    stateId   <- freshID idAllocator
    lastChild <- newSTRef '\0'
    accept    <- newSTRef True
    children  <- H.new
    pure DFAState{..}
  go (c:cs) = do
    stateId   <- freshID idAllocator
    lastChild <- newSTRef c
    accept    <- newSTRef False
    children  <- H.new
    H.insert children c =<< go cs
    pure DFAState{..}
{-# INLINABLE wordDFA #-}

replaceOrRegister :: IDAllocator s -> STRef s [DFAState s] -> DFAState s -> ST s ()
replaceOrRegister idAllocator register = go where
  go DFAState{..} = do
    lastChildChar <- readSTRef lastChild
    traverse_ <&> (H.lookup children lastChildChar >>=) $ \child@DFAState{stateId=childStateId} -> do
      go child
      readSTRef register >>= findM (equiv child) >>= \case
        Just q  -> do H.insert children lastChildChar q
                      deleteID idAllocator childStateId
        Nothing -> modifySTRef' register (child:)
{-# INLINABLE  replaceOrRegister #-}

fromAscST :: Foldable t => t String -> ST s (DFAState s)
fromAscST words = do
  register     <- newSTRef []
  idAllocator  <- IDAllocator <$> newSTRef 0 <*> newSTRef mempty
  initialState <- do stateId     <- freshID idAllocator
                     lastChild   <- newSTRef '\0'
                     accept      <- newSTRef False
                     children    <- H.new
                     pure DFAState{..}

  for_ words $ \word -> do
    (lastState, currentSuffix) <- walkPrefix initialState  word
    replaceOrRegister idAllocator register lastState
    dfaInsert idAllocator lastState currentSuffix
  replaceOrRegister idAllocator register initialState
  pure initialState
{-# SPECIALIZE fromAscST :: [String] -> ST s (DFAState s) #-}
