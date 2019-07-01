{-# LANGUAGE LambdaCase #-}

module CLI.BuildTextSet (main) where

import qualified Text.Set      as TS
import qualified Text.Set.Test as TS

import Test.QuickCheck

import System.Environment

main :: IO ()
main = getArgs >>= \case
  ["sorted"]   -> dumpBuilt TS.fromAsc
  ["unsorted"] -> dumpBuilt TS.fromFoldable
  ["test"]     -> quickCheck TS.prop_TextSet
  _            -> do
    name <- getProgName
    putStrLn $ "Usage: " ++ name ++ " (sorted|unsorted|test)"
  where
    dumpBuilt from = print . TS.textSetDAFSA . from . lines =<< getContents
    {-# INLINE dumpBuilt #-}
