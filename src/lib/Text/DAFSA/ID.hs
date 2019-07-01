{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, RecordWildCards #-}

module Text.DAFSA.ID (
  -- * State IDs
  ID(..),
  -- * Allocating fresh IDs
  IDAllocator(..),
  newIDAllocator,
  freshID,
  deleteID
) where

import Data.Hashable

import           Data.Set (Set)
import qualified Data.Set as S

import Control.Monad.ST.Strict
import Data.STRef.Strict

newtype ID = ID { getID :: Int }
  deriving (Eq, Ord, Enum, Bounded, Num, Real, Integral, Hashable, Show, Read)

data IDAllocator s = IDAllocator { nextId   :: !(STRef s ID)
                                 , freeList :: !(STRef s (Set ID)) }
                   deriving Eq

newIDAllocator :: ST s (IDAllocator s)
newIDAllocator = do
  nextId   <- newSTRef 0
  freeList <- newSTRef S.empty
  pure IDAllocator{..}

freshID :: IDAllocator s -> ST s ID
freshID IDAllocator{..} =
  S.minView <$> readSTRef freeList >>= \case
    Just (fresh, freeList') -> fresh <$ writeSTRef freeList freeList'
    Nothing                 -> readSTRef nextId <* modifySTRef' nextId (+1)

deleteID :: IDAllocator s -> ID -> ST s ()
deleteID IDAllocator{..} = modifySTRef' freeList . S.insert
