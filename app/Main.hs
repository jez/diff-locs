{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           Control.Monad      (when)
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
    InputFromFile filename -> IO.openFile filename IO.ReadMode
    InputFromStdin         -> do
      isTTY <- IO.hIsTerminalDevice IO.stdin
      when isTTY $ do
        IO.hPutStrLn IO.stderr "Warning: reading from stdin, which is a tty."
      return IO.stdin

  run $ Config {configFileIn = fileIn, configWhichLines = optionsWhichLines}
