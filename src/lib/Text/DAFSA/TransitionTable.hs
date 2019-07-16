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

import           Data.Text (Text)
import qualified Data.Text as T

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
  let go DFAState{..} = unlessM ((coerce stateID `S.member`) <$> readSTRef seen) $ do
        modifySTRef' seen $ coerce S.insert stateID
        whenM (readSTRef accept) $ modifySTRef' acceptStates (coerce S.insert stateID)
        flip H.mapM_ children $ \(c,child@DFAState{stateID=childID}) -> do
          modifySTRef transitions (M.insert (stateID, c) childID)
          go child
  go state0
  DAFSA <$> readSTRef transitions <*> readSTRef acceptStates

fromAsc :: Foldable t => t Text -> DAFSA
fromAsc words = runST $ graphToTransitionTable =<< fromAscST words
{-# INLINABLE  fromAsc #-}
{-# SPECIALIZE fromAsc :: [Text] -> DAFSA #-}

lookup :: Text -> DAFSA -> Maybe ID
lookup w DAFSA{..} = foldlMT (\q c -> M.lookup (q,c) transitions) 0 w
  where -- Stole the implementation from "Data.Foldable"
        foldlMT f z0 xs = T.foldr f' return xs z0
          where f' x k z = f z x >>= k
{-# INLINABLE lookup #-}
