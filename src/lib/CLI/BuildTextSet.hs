{-# LANGUAGE LambdaCase #-}

module CLI.BuildTextSet (
  main,
  -- * CLI interface
  Command(..), commands, commandsInfo
) where

import Data.Foldable

import qualified Text.Set      as TS
import qualified Text.Set.Test as TS

import Test.QuickCheck

import Options.Applicative

data Command = SortedCmd
             | UnsortedCmd
             | TestCmd  Word
             | TestsCmd Word
             deriving (Eq, Ord, Show, Read)

commands :: Parser Command
commands = hsubparser $  trivialCommand "sorted"   SortedCmd   "Generate a TextSet from a sorted input list"
                      <> trivialCommand "unsorted" UnsortedCmd "Generate a TextSet from an unsorted input list"
                      <> testingCommand "test"     TestCmd     "Run the single aggregate test for TextSet"
                      <> testingCommand "tests"    TestsCmd    "Run the individual tests for TextSet"
  where
    makeCommand parser name cmd desc = command name . info (parser cmd) $ progDesc desc
    trivialCommand = makeCommand pure
    testingCommand = makeCommand . flip fmap $
                       argument auto $  metavar "TESTS"
                                     <> help "Number of test inputs to generate"
                                     <> value 100
                                     <> showDefault

commandsInfo :: ParserInfo Command
commandsInfo = info (helper <*> commands) $  fullDesc
                                          <> progDesc "Generate TextSets or test TextSet behavior"

main :: IO ()
main = execParser commandsInfo >>= \case
  SortedCmd   -> dumpBuilt TS.fromAsc
  UnsortedCmd -> dumpBuilt TS.fromFoldable
  TestCmd  n  -> quickCheckN n TS.prop_TextSet
  TestsCmd n  -> traverse_ (\(name,test) -> putStrLn (name ++ ":") *> quickCheckN n test)
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
