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
) where

import Data.List (sort)

import           Data.Set (Set)
import           Text.Set (TextSet)
import qualified Data.Set as S
import qualified Text.Set as TS

import Test.QuickCheck

--------------------------------------------------------------------------------
-- Nonempty sorted lists

newtype NonEmptySortedList a = NonEmptySorted { getNonEmptySorted :: [a] }
                             deriving (Eq, Ord, Show, Read)

instance (Arbitrary a, Ord a) => Arbitrary (NonEmptySortedList a) where
  arbitrary = NonEmptySorted  <$> orderedList `suchThat` (not . null)
  shrink    = map (NonEmptySorted . sort) . shrink . getNonEmptySorted

------------------------------------------------------------------------------------------
-- Building blocks (module-local)

test_IsEmpty :: TextSet -> String -> Bool
test_IsEmpty tset =
  (`TS.notMember` tset)
{-# INLINE test_IsEmpty #-}

test_HasMembers :: Set String -> TextSet -> Property
test_HasMembers sset tset =
  forAll (choose (0, length sset - 1)) ((`TS.member` tset) . (`S.elemAt` sset))
{-# INLINE test_HasMembers #-}

test_SameMembership :: Set String -> TextSet -> String -> Bool
test_SameMembership sset tset = \word ->
  word `S.member` sset == word `TS.member` tset
{-# INLINE test_SameMembership #-}

test_OnSetAndTextSet :: Testable prop
                     => (Set String -> TextSet -> prop)
                     -> SortedList String
                     -> Property
test_OnSetAndTextSet test =
  property . (test <$> S.fromAscList <*> TS.fromAsc) . getSorted
{-# INLINE test_OnSetAndTextSet #-}

test_OnNonemptySetAndTextSet :: Testable prop
                             => (Set String -> TextSet -> prop)
                             -> NonEmptySortedList String
                             -> Property
test_OnNonemptySetAndTextSet test =
  test_OnSetAndTextSet test . Sorted . getNonEmptySorted
{-# INLINE test_OnNonemptySetAndTextSet #-}

--------------------------------------------------------------------------------
-- Properties

prop_TextSet :: SortedList String -> Property
prop_TextSet (Sorted words) =
  let sset = S.fromAscList words
      tset = TS.fromAsc    words
  in if null words
     then property $ test_IsEmpty tset
     else test_HasMembers     sset tset .&&.
          test_SameMembership sset tset

prop_ConstantEmptyTextSet :: String -> Bool
prop_ConstantEmptyTextSet = (`TS.notMember` TS.empty)

prop_FromAscEmptyTextSet :: String -> Bool
prop_FromAscEmptyTextSet = (`TS.notMember` TS.fromAsc [])

prop_TextSetMembers :: NonEmptySortedList String -> Property
prop_TextSetMembers = test_OnNonemptySetAndTextSet test_HasMembers

prop_TextSetAll :: NonEmptySortedList String -> Property
prop_TextSetAll = test_OnNonemptySetAndTextSet test_SameMembership
