{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}

module DiffLocs.Options where

import           Control.Monad         (when)
import           Data.Version          (Version, showVersion)
import           System.Console.Docopt
import qualified System.Environment    as Env
import           System.Exit           (exitSuccess)

import           DiffLocs.Types

patterns :: Docopt
patterns =
  [docopt|
List the filename:line pairs involved in a diff.

Usage:
  diff-locs [options] [<file>]

Arguments:
  <file>                 The diff file to read from. Must be in unified (-u)
                         format. [default: stdin]

Options:
  -a, --all              Print both lines added and removed by the diff.
                         Default is only lines added (and/or modified).
  -h, --help             Show this help message
  -v, --version          Show version information

Output format:
  If printing only added lines:

      filename.ext:23

  If printing added and removed lines:

      -filename.ext:22
      -filename.ext:23
      +filename.ext:22

|]

hasOption :: Arguments -> String -> Bool
hasOption args opt = isPresent args (longOption opt)

parseArgv :: Version -> IO Options
parseArgv version = do
  args <- parseArgsOrExit patterns =<< Env.getArgs

  when (args `hasOption` "help") $ do
    putStr $ usage patterns
    exitSuccess

  when (args `hasOption` "version") $ do
    putStrLn $ showVersion version
    exitSuccess

  let optionsInput =
        case getArg args (argument "file") of
          Just filename -> InputFromFile filename
          Nothing       -> InputFromStdin

  let optionsWhichLines =
        case args `hasOption` "all" of
          True  -> LinesAddedAndRemoved
          False -> LinesAdded

  return $ Options { optionsInput, optionsWhichLines }
