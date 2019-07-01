{-# LANGUAGE RecordWildCards #-}

module Text.DAFSA.TransitionTable (
  DAFSA(..),
  graphToTransitionTable,
  fromAsc,
  lookup
) where

import Prelude hiding (lookup)

import Data.Foldable

import qualified Data.Set             as S
import qualified Data.Map.Strict      as M
import qualified Data.HashTable.Class as H

import Control.Monad.Util
import Control.Monad.ST.Strict
import Data.STRef.Strict

import Text.DAFSA.ID
import Text.DAFSA.Graph

data DAFSA = DAFSA { transitions  :: !(M.Map (ID, Char) ID)
                   , acceptStates :: !(S.Set ID) }
           deriving (Eq, Ord, Show, Read)

graphToTransitionTable :: DFAState s -> ST s DAFSA
graphToTransitionTable state0 = do
  seen         <- newSTRef S.empty
  transitions  <- newSTRef M.empty
  acceptStates <- newSTRef S.empty
  let go DFAState{..} = unlessM ((stateId `S.member`) <$> readSTRef seen) $ do
        modifySTRef' seen (S.insert stateId)
        whenM (readSTRef accept) $ modifySTRef' acceptStates (S.insert stateId)
        flip H.mapM_ children $ \(c,child@DFAState{stateId=childId}) -> do
          modifySTRef transitions (M.insert (stateId, c) childId)
          go child
  go state0
  DAFSA <$> readSTRef transitions <*> readSTRef acceptStates

fromAsc :: Foldable t => t String -> DAFSA
fromAsc words = runST $ graphToTransitionTable =<< fromAscST words
{-# INLINABLE  fromAsc #-}
{-# SPECIALIZE fromAsc :: [String] -> DAFSA #-}

lookup :: String -> DAFSA -> Maybe ID
lookup w DAFSA{..} = foldlM (\q c -> M.lookup (q,c) transitions) 0 w
{-# INLINABLE lookup #-}
