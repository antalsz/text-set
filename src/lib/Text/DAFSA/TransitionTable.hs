{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RecordWildCards #-}

module Text.DAFSA.TransitionTable (
  DAFSA(..),
  graphToTransitionTable,
  fromAsc,
  lookup
) where

import Prelude hiding (lookup)

import GHC.Generics
import Data.Coerce
import Control.DeepSeq

import Data.Foldable

import qualified Data.IntSet          as S
import qualified Data.Map.Strict      as M
import qualified Data.HashTable.Class as H

import Control.Monad.Util
import Control.Monad.ST.Strict
import Data.STRef.Strict

import Text.DAFSA.ID
import Text.DAFSA.Graph

data DAFSA = DAFSA { transitions  :: !(M.Map (ID, Char) ID)
                   , acceptStates :: !S.IntSet }
           deriving (Eq, Ord, Show, Read, Generic, NFData)

graphToTransitionTable :: DFAState s -> ST s DAFSA
graphToTransitionTable state0 = do
  seen         <- newSTRef mempty
  transitions  <- newSTRef mempty
  acceptStates <- newSTRef mempty
  let go DFAState{..} = unlessM ((coerce stateId `S.member`) <$> readSTRef seen) $ do
        modifySTRef' seen $ coerce S.insert stateId
        whenM (readSTRef accept) $ modifySTRef' acceptStates (coerce S.insert stateId)
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
