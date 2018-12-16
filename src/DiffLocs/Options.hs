{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}

module DiffLocs.Options where

import           Control.Monad         (when)
import           System.Console.Docopt
import qualified System.Environment    as Env
import           System.Exit           (exitSuccess)

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

data Input
  = FromStdin
  | FromFile FilePath

data WhichLines
  = LinesAdded
  | LinesAddedAndRemoved

data Options = Options
  { optionsInput      :: Input
  , optionsWhichLines :: WhichLines
  }

hasOption :: Arguments -> String -> Bool
hasOption args opt = isPresent args (longOption opt)

parseArgv :: String -> IO Options
parseArgv version = do
  args <- parseArgsOrExit patterns =<< Env.getArgs

  when (args `hasOption` "help") $ do
    putStr $ usage patterns
    exitSuccess

  when (args `hasOption` "version") $ do
    putStrLn version
    exitSuccess

  let optionsInput =
        case getArg args (argument "file") of
          Just filename -> FromFile filename
          Nothing       -> FromStdin

  let optionsWhichLines =
        case args `hasOption` "all" of
          True  -> LinesAddedAndRemoved
          False -> LinesAdded

  return $ Options { optionsInput, optionsWhichLines }
