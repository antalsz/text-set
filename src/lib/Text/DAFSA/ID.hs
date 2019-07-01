{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, DeriveAnyClass, DerivingStrategies,
             LambdaCase, RecordWildCards #-}

module Text.DAFSA.ID (
  -- * State IDs
  ID(..),
  -- * Allocating fresh IDs
  IDAllocator(..),
  newIDAllocator,
  freshID,
  deleteID
) where

import GHC.Generics
import Data.Coerce
import Control.DeepSeq

import Data.Hashable

import           Data.IntSet (IntSet)
import qualified Data.IntSet as S

import Control.Monad.ST.Strict
import Data.STRef.Strict

newtype ID = ID { getID :: Int }
           deriving stock    (Show, Read, Generic)
           deriving newtype  (Eq, Ord, Enum, Bounded, Num, Real, Integral, Hashable)
           deriving anyclass (NFData)

data IDAllocator s = IDAllocator { nextID   :: !(STRef s ID)
                                 , freeList :: !(STRef s IntSet) }
                   deriving (Eq, Generic, NFData)

newIDAllocator :: ST s (IDAllocator s)
newIDAllocator = do
  nextID   <- newSTRef 0
  freeList <- newSTRef S.empty
  pure IDAllocator{..}

freshID :: IDAllocator s -> ST s ID
freshID IDAllocator{..} =
  S.minView <$> readSTRef freeList >>= \case
    Just (fresh, freeList') -> ID fresh <$ writeSTRef freeList freeList'
    Nothing                 -> readSTRef nextID <* modifySTRef' nextID (+1)

deleteID :: IDAllocator s -> ID -> ST s ()
deleteID IDAllocator{..} = modifySTRef' freeList . coerce S.insert
