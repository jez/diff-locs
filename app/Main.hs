{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           DiffLocs.InputLoop
import           DiffLocs.Options
import qualified System.IO          as IO

main :: IO ()
main = do
  Options { optionsInput, optionsWhichLines } <- parseArgv "0.1.0"

  fileIn <- case optionsInput of
    -- Leak the file handle because we're short lived anyways
    FromFile filename -> IO.openFile filename IO.ReadMode
    FromStdin         -> return IO.stdin

  -- 'prevLine' is somewhat of an implementation detail of run... /shrug
  let prevLine = Nothing

  run fileIn optionsWhichLines prevLine
