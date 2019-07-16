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

{-# LANGUAGE DeriveGeneric,
             TypeApplications, ScopedTypeVariables,
             TupleSections, LambdaCase, RecordWildCards, OverloadedStrings #-}

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

import GHC.Generics

import Data.Functor
import Data.Foldable

import           Data.Text (Text)
import qualified Data.Text as T


import           Data.Hashable
import           Data.HashTable.Class    (HashTable)
import qualified Data.HashTable.Class    as H
import qualified Data.HashTable.ST.Basic as Basic
  -- I'd rather use cuckoo hashing, but there's a bug in the library with
  -- `mutate`/`mutateST` where it loses inserts sometimes -- see
  -- <https://github.com/gregorycollins/hashtables/issues/55 issue #55>

import Control.Monad.Util
import Control.Monad.ST.Strict
import Data.STRef.Strict

import Text.DAFSA.ID

data DFAState s = DFAState { stateID   :: !ID
                           , lastChild :: !(STRef s Char)
                           , accept    :: !(STRef s Bool)
                           , children  :: !(Basic.HashTable s Char (DFAState s)) }
                deriving (Generic)

newEmptyDFAState :: IDAllocator s -> Bool -> ST s (DFAState s)
newEmptyDFAState idAllocator acc = do
  stateID   <- freshID idAllocator
  lastChild <- newSTRef minBound
  accept    <- newSTRef acc
  children  <- H.new
  pure DFAState{..}
{-# INLINABLE newEmptyDFAState #-}

-- Module-local
equiv_children :: HashTable h => h s Char (DFAState s) -> h s Char (DFAState s) -> ST s Bool
equiv_children kids1 kids2 =
  let this `subset` that =
        H.foldM (\b (c,q) -> pure b `andM` any ((stateID q ==) . stateID) <$> H.lookup that c) True this
      {-# INLINE subset #-}
  in (kids1 `subset` kids2) `andM` (kids2 `subset` kids1)
{-# INLINE equiv_children #-}

equiv :: DFAState s -> DFAState s -> ST s Bool
equiv state1 state2 =
  (==) <$> readSTRef (accept state1) <*> readSTRef (accept state2) `andM`
  children state1 `equiv_children` children state2

-- TODO more efficient?
walkPrefix :: DFAState s -> Text -> ST s (DFAState s, Text)
walkPrefix state@DFAState{..} w = case T.uncons w of
  Nothing     -> pure (state, "")
  Just (c,cs) -> H.lookup children c >>= \case
                   Just state' -> walkPrefix state' cs
                   Nothing     -> pure (state, w)

insertNewOrElse :: (HashTable h, Eq k, Hashable k) => h s k v -> k -> ST s v -> (v -> ST s ()) -> ST s ()
insertNewOrElse h k whenAbsent whenPresent =
  H.mutateST h k $ \case
    Just v  -> (Just v,)    <$> whenPresent v
    Nothing -> (,()) . Just <$> whenAbsent
{-# INLINABLE insertNewOrElse #-}

dfaInsert :: IDAllocator s -> DFAState s -> Text -> ST s ()
dfaInsert idAllocator = go where
  go DFAState{..} = T.uncons <&> \case
    Nothing     -> writeSTRef accept True
    Just (c,cs) -> insertNewOrElse children c
                     (modifySTRef' lastChild (max c) *> wordDFA idAllocator cs)
                     (\state' -> dfaInsert idAllocator state' cs)
{-# INLINABLE dfaInsert #-}

wordDFA :: IDAllocator s -> Text -> ST s (DFAState s)
wordDFA idAllocator = go where
  go = T.uncons <&> \case
     Nothing     -> newEmptyDFAState idAllocator True
     Just (c,cs) -> do q <- newEmptyDFAState idAllocator False
                       H.insert (children q) c =<< go cs
                       pure q
{-# INLINABLE wordDFA #-}

replaceOrRegister :: IDAllocator s -> STRef s [DFAState s] -> DFAState s -> ST s ()
replaceOrRegister idAllocator register = go where
  go DFAState{..} = do
    lastChildChar <- readSTRef lastChild
    traverse_ <&> (H.lookup children lastChildChar >>=) $ \child@DFAState{stateID=childStateID} -> do
      go child
      readSTRef register >>= findM (equiv child) >>= \case
        Just q  -> do H.insert children lastChildChar q
                      deleteID idAllocator childStateID
        Nothing -> modifySTRef' register (child:)
{-# INLINABLE  replaceOrRegister #-}

fromAscST :: Foldable t => t Text -> ST s (DFAState s)
fromAscST words = do
  register     <- newSTRef []
  idAllocator  <- newIDAllocator
  initialState <- newEmptyDFAState idAllocator False

  for_ words $ \word -> do
    (lastState, currentSuffix) <- walkPrefix initialState  word
    replaceOrRegister idAllocator register lastState
    dfaInsert idAllocator lastState currentSuffix
  replaceOrRegister idAllocator register initialState
  pure initialState
{-# SPECIALIZE fromAscST :: [Text] -> ST s (DFAState s) #-}
