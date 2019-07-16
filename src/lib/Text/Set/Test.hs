{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Text.Set.Test (
  -- * Unified property
  prop_TextSet,
  -- * Component properties
  prop_ConstantEmptyTextSet,
  prop_FromAscEmptyTextSet,
  prop_TextSetMembers,
  prop_TextSetAll,

  -- * Nonempty sorted lists
  NonEmptySortedList(..)

  -- * Orphan instances
  -- | This module provides an orphan instance for @'Arbitrary' 'Text'@
) where

import GHC.Generics
import Control.DeepSeq

import Data.List (sort)

import           Data.Text (Text)
import qualified Data.Text as T

import           Data.Set (Set)
import           Text.Set (TextSet)
import qualified Data.Set as S
import qualified Text.Set as TS

import Test.QuickCheck

--------------------------------------------------------------------------------
-- Orphan instances

instance Arbitrary Text where
  arbitrary = T.pack <$> listOf arbitrary
  shrink    = map T.pack . shrink . T.unpack

--------------------------------------------------------------------------------
-- Nonempty sorted lists

newtype NonEmptySortedList a = NonEmptySorted { getNonEmptySorted :: [a] }
                             deriving (Eq, Ord, Show, Read, Generic, NFData)

instance (Arbitrary a, Ord a) => Arbitrary (NonEmptySortedList a) where
  arbitrary = NonEmptySorted  <$> orderedList `suchThat` (not . null)
  shrink    = map (NonEmptySorted . sort) . shrink . getNonEmptySorted

------------------------------------------------------------------------------------------
-- Building blocks (module-local)

test_IsEmpty :: TextSet -> Text -> Bool
test_IsEmpty tset =
  (`TS.notMember` tset)
{-# INLINE test_IsEmpty #-}

test_HasMembers :: Set Text -> TextSet -> Property
test_HasMembers sset tset =
  forAll (choose (0, length sset - 1)) ((`TS.member` tset) . (`S.elemAt` sset))
{-# INLINE test_HasMembers #-}

test_SameMembership :: Set Text -> TextSet -> Text -> Bool
test_SameMembership sset tset = \word ->
  word `S.member` sset == word `TS.member` tset
{-# INLINE test_SameMembership #-}

test_OnSetAndTextSet :: Testable prop
                     => (Set Text -> TextSet -> prop)
                     -> SortedList Text
                     -> Property
test_OnSetAndTextSet test =
  property . (test <$> S.fromAscList <*> TS.fromAsc) . getSorted
{-# INLINE test_OnSetAndTextSet #-}

test_OnNonemptySetAndTextSet :: Testable prop
                             => (Set Text -> TextSet -> prop)
                             -> NonEmptySortedList Text
                             -> Property
test_OnNonemptySetAndTextSet test =
  test_OnSetAndTextSet test . Sorted . getNonEmptySorted
{-# INLINE test_OnNonemptySetAndTextSet #-}

--------------------------------------------------------------------------------
-- Properties

prop_TextSet :: SortedList Text -> Property
prop_TextSet (Sorted words) =
  let sset = S.fromAscList words
      tset = TS.fromAsc    words
  in if null words
     then property $ test_IsEmpty tset
     else test_HasMembers     sset tset .&&.
          test_SameMembership sset tset

prop_ConstantEmptyTextSet :: Text -> Bool
prop_ConstantEmptyTextSet = (`TS.notMember` TS.empty)

prop_FromAscEmptyTextSet :: Text -> Bool
prop_FromAscEmptyTextSet = (`TS.notMember` TS.fromAsc [])

prop_TextSetMembers :: NonEmptySortedList Text -> Property
prop_TextSetMembers = test_OnNonemptySetAndTextSet test_HasMembers

prop_TextSetAll :: NonEmptySortedList Text -> Property
prop_TextSetAll = test_OnNonemptySetAndTextSet test_SameMembership
