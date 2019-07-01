{-|
Module      : Text.DAFSA.Graph.Inspect
Description : For debugging: Display a DAFSA graph; i.e., a `DFAState` and its children
Copyright   : Copyright © 2019 Antal Spector-Zabusky
License     : MIT
Maintainer  : Antal Spector-Zabusky <antal.b.sz@gmail.com>

This module is for debugging purposes
-}

{-# LANGUAGE RankNTypes, RecordWildCards #-}

module Text.DAFSA.Graph.Inspect (
  -- * Frozen/pure DFA states
  DFAState'(..),
  freezeDFAState, freezeDFAState',
  
  -- * Displaying the structure
  drawDFAState',
  
  -- * 'Tree' structure
  toTree, NodeLabel(..), drawNodeLabel
) where

import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M
import qualified Data.HashTable.Class as H

import Data.Tree

import Control.Monad.ST.Strict
import Data.STRef.Strict

import Text.DAFSA.ID
import Text.DAFSA.Graph

data DFAState' = DFAState' { stateId'   :: !ID
                           , lastChild' :: !Char
                           , accept'    :: !Bool
                           , children'  :: !(Map Char DFAState') }
               deriving (Eq, Ord, Show, Read)


freezeDFAState' :: DFAState s -> ST s DFAState'
freezeDFAState' DFAState{..} =
  DFAState' stateId
        <$> readSTRef lastChild
        <*> readSTRef accept
        <*> (fmap M.fromList . traverse (traverse freezeDFAState') =<< H.toList children)

freezeDFAState :: (forall s. ST s (DFAState s)) -> DFAState'
freezeDFAState q = runST $ freezeDFAState' =<< q

data NodeLabel = NodeLabel { nodeFromChar  :: !(Maybe Char)
                           , nodeStateID   :: !ID
                           , nodeLastChild :: !(Maybe Char)
                           , nodeAccept    :: !Bool }
               deriving (Eq, Ord, Show, Read)

toTree :: DFAState' -> Tree NodeLabel
toTree = go Nothing where
  go fromChar DFAState'{..} =
    Node (NodeLabel fromChar
                    stateId'
                    (if null children' && lastChild' == minBound then Nothing else Just lastChild')
                    accept')
         (map (uncurry $ go . Just) $ M.toList children')

drawNodeLabel :: NodeLabel -> String
drawNodeLabel NodeLabel{..} =
  concat [ foldMap (\c -> "-" ++ c : "-> ") nodeFromChar
         , "(" , ['('|nodeAccept]
         , show $ getID nodeStateID
         , foldMap (\c -> " | " ++ [c]) nodeLastChild
         , [')'|nodeAccept], ")" ]

drawDFAState' :: DFAState' -> String
drawDFAState' = drawTree . fmap drawNodeLabel . toTree
