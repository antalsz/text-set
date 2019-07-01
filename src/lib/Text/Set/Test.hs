module Text.Set.Test (
  prop_TextSet
) where

import qualified Data.Set as S
import qualified Text.Set as TS

import Test.QuickCheck

prop_TextSet :: SortedList String -> Property
prop_TextSet (Sorted words) =
  let sset = S.fromAscList words
      tset = TS.fromAsc    words
  in if null words
     then forAllShrink arbitrary shrink (`TS.notMember` tset)
     else forAll       (choose (0, length sset - 1)) ((`TS.member` tset) . (`S.elemAt` sset)) .&&.
          forAllShrink arbitrary shrink (\word -> word `S.member` sset == word `TS.member` tset)
