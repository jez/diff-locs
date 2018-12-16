{-# LANGUAGE NamedFieldPuns #-}
module Main where

import qualified System.IO          as IO

import           DiffLocs.InputLoop
import           DiffLocs.Options
import           DiffLocs.Types
import           Paths_diff_locs    (version)

main :: IO ()
main = do
  Options { optionsInput, optionsWhichLines } <- parseArgv version

  fileIn <- case optionsInput of
    -- Leak the file handle because we're short lived anyways
    FromFile filename -> IO.openFile filename IO.ReadMode
    FromStdin         -> return IO.stdin

  run $ Config {configFileIn = fileIn, configWhichLines = optionsWhichLines}
