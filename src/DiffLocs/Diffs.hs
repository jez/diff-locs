{-# LANGUAGE OverloadedStrings #-}

module DiffLocs.Diffs where

import           Data.Attoparsec.Text

import           DiffLocs.Types

-- This file aims to parse both git diff output and diff -u output.
-- There are some idiosyncrasises between both, which we attempt to document.

-- diff -u shows a timestamp after a tab character after the filename.
isEndOfLineOrTab :: Char -> Bool
isEndOfLineOrTab '\t' = True
isEndOfLineOrTab c    = isEndOfLine c

-- The a/ and b/ prefix is missing when a file is added or deleted from git.
fromFile :: Parser FromFile
fromFile = do
  _        <- string "--- "
  _        <- option "" (string "a/")
  filename <- takeTill isEndOfLineOrTab
  _        <- takeTill isEndOfLine
  return $ FromFile filename

toFile :: Parser ToFile
toFile = do
  _        <- string "+++ "
  _        <- option "" (string "b/")
  filename <- takeTill isEndOfLineOrTab
  _        <- takeTill isEndOfLine
  return $ ToFile filename

-- diff -u doesn't have count information
hunkCount :: Parser (Maybe Int)
hunkCount = do
  _ <- string ","
  n <- decimal
  return $ Just n

hunkInfo :: Parser HunkInfo
hunkInfo = do
  _     <- string "@@ -"
  line1 <- FromLine <$> decimal
  _     <- option Nothing hunkCount
  _     <- string " +"
  line2 <- ToLine <$> decimal
  _     <- option Nothing hunkCount
  _     <- string " @@"
  _     <- takeTill isEndOfLine
  return $ HunkInfo line1 line2
