{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           Control.Monad         (when)
import qualified System.IO             as IO
import           System.Posix.IO       (stdInput)
import           System.Posix.Terminal (queryTerminal)

import           DiffLocs.InputLoop
import           DiffLocs.Options
import           DiffLocs.Types
import           Paths_diff_locs       (version)

main :: IO ()
main = do
  Options { optionsInput, optionsWhichLines } <- parseArgv version

  fileIn <- case optionsInput of
    -- Leak the file handle because we're short lived anyways
    InputFromFile filename -> IO.openFile filename IO.ReadMode
    InputFromStdin         -> do
      isTTY <- queryTerminal stdInput
      when isTTY $ do
        IO.hPutStrLn IO.stderr "Warning: reading from stdin, which is a tty."
      return IO.stdin

  run $ Config {configFileIn = fileIn, configWhichLines = optionsWhichLines}
