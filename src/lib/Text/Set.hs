{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TypeApplications, ScopedTypeVariables #-}

module Text.Set (
  -- * The 'TextSet' data type (a 'DAFSA')
  TextSet(..),
  -- * Construction
  empty, singleton, fromAsc, fromFoldable,
  -- * Query
  member, notMember, null,
) where

import Prelude hiding (null, lookup)

import GHC.Generics
import Data.Coerce
import Control.DeepSeq

import Data.Foldable hiding (null)
import Data.List (sort)

import Data.Text (Text)

import qualified Data.IntSet as S

import Text.DAFSA.ID (ID(..))
import Text.DAFSA.TransitionTable hiding (fromAsc)
import qualified Text.DAFSA.TransitionTable as DAFSA

newtype TextSet = TextSet { textSetDAFSA :: DAFSA } deriving (Generic, NFData)

empty :: TextSet
empty = TextSet $ DAFSA mempty mempty
{-# INLINABLE empty #-}

singleton :: Text -> TextSet
singleton = fromAsc . (:[])
{-# INLINABLE singleton #-}

fromAsc :: forall t. Foldable t => t Text -> TextSet
fromAsc = coerce $ DAFSA.fromAsc @t
{-# INLINABLE  fromAsc #-}
{-# SPECIALIZE fromAsc :: [Text] -> TextSet #-}

fromFoldable :: Foldable t => t Text -> TextSet
fromFoldable = fromAsc . sort . toList
{-# INLINABLE fromFoldable #-}

member :: Text -> TextSet -> Bool
member s (TextSet dafsa) = any (coerce (`S.member` acceptStates dafsa)) $ lookup s dafsa
{-# INLINABLE member #-}

notMember :: Text -> TextSet -> Bool
notMember = \s set -> not $ s `member` set
{-# INLINABLE notMember #-}

null :: TextSet -> Bool
null = S.null . acceptStates . textSetDAFSA
{-# INLINABLE null #-}
