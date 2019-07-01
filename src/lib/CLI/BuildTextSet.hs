{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TypeApplications, LambdaCase, NumDecimals #-}

module CLI.BuildTextSet (
  main,
  -- * CLI interface
  Command(..), commands, commandsInfo,
  -- * Benchmarks
  benchmarks
) where

import Unsafe.Coerce
import GHC.Generics
import Control.DeepSeq

import Data.Foldable
import Data.List (sort)

import qualified Text.Set      as TS
import qualified Text.Set.Test as TS

import Test.QuickCheck

import           Criterion
import qualified Criterion.Main         as Criterion
import qualified Criterion.Main.Options as Criterion

import Options.Applicative

data Command = SortedCmd
             | UnsortedCmd
             | BenchmarkCmd Criterion.Mode
             | TestCmd  Word
             | TestsCmd Word
             deriving (Eq, Show, Read, Generic)

commands :: Parser Command
commands = subparser
  $  trivialCommand   "sorted"    SortedCmd    "Generate a TextSet from a sorted input list"
  <> trivialCommand   "unsorted"  UnsortedCmd  "Generate a TextSet from an unsorted input list"
  <> benchmarkCommand "benchmark" BenchmarkCmd "Run the TextSet benchmarks"
  <> testingCommand   "test"      TestCmd      "Run the single aggregate test for TextSet"
  <> testingCommand   "tests"     TestsCmd     "Run the individual tests for TextSet"
  where
    makeCommand parser name cmd desc = command name . info (helper <*> parser cmd) $ progDesc desc
    trivialCommand  = makeCommand pure
    testingCommand  = makeCommand . flip fmap $
                        argument auto $  metavar "TESTS"
                                      <> help "Number of test inputs to generate"
                                      <> value 100
                                      <> showDefault
    benchmarkCommand name cmd desc =
      command name $ cmd <$> progDesc desc `unsafeApplyInfoMod` Criterion.describe Criterion.defaultConfig

    -- I checked the docs, and `InfoMod` is a newtype wrapper.  I want to get
    -- `progDesc` attached to the `Criterion` output, and this is the only way I
    -- can see to do it.
    unsafeApplyInfoMod :: InfoMod a -> (ParserInfo a -> ParserInfo a)
    unsafeApplyInfoMod = unsafeCoerce

commandsInfo :: ParserInfo Command
commandsInfo = info (helper <*> commands) $  fullDesc
                                          <> progDesc "Generate TextSets or test TextSet behavior"

benchmarks :: [Benchmark]
benchmarks = rnf numbers `seq` [create]
  where
    upper   = 1e5
    numbers = sort $ map (show @Int) [0..upper-1] 
    create  = bgroup "create" . flip map [1e3,2e3..upper] $ \k ->
                bench (show k) . nf TS.fromAsc $ take k numbers

main :: IO ()
main = execParser commandsInfo >>= \case
  SortedCmd          -> dumpBuilt TS.fromAsc
  UnsortedCmd        -> dumpBuilt TS.fromFoldable
  BenchmarkCmd cmode -> Criterion.runMode cmode benchmarks
  TestCmd  n         -> quickCheckN n TS.prop_TextSet
  TestsCmd n         -> traverse_ (\(name,test) -> putStrLn (name ++ ":") *> quickCheckN n test)
                                  [ "prop_ConstantEmptyTextSet" -: TS.prop_ConstantEmptyTextSet
                                  , "prop_FromAscEmptyTextSet"  -: TS.prop_FromAscEmptyTextSet
                                  , "prop_TextSetMembers"       -: TS.prop_TextSetMembers
                                  , "prop_TextSetAll"           -: TS.prop_TextSetAll]
  where
    dumpBuilt from = print . TS.textSetDAFSA . from . lines =<< getContents
    {-# INLINE dumpBuilt #-}
    
    name -: prop = (name, property prop)
    {-# INLINE (-:) #-}

    quickCheckN n = quickCheckWith stdArgs{maxSuccess = fromIntegral n}
    {-# INLINE quickCheckN #-}
