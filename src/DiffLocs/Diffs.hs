{-# LANGUAGE OverloadedStrings #-}

module DiffLocs.Diffs where

import           Data.Attoparsec.Text

import           DiffLocs.Types

fromFile :: Parser FromFile
fromFile = do
  _        <- string "--- "
  _        <- option "" (string "a/")
  filename <- takeTill isEndOfLine
  return $ FromFile filename

toFile :: Parser ToFile
toFile = do
  _        <- string "+++ "
  _        <- option "" (string "b/")
  filename <- takeTill isEndOfLine
  return $ ToFile filename

hunkInfo :: Parser HunkInfo
hunkInfo = do
  _     <- string "@@ -"
  line1 <- FromLine <$> decimal
  _     <- string ","
  _     <- (decimal :: Parser Int)
  _     <- string " +"
  line2 <- ToLine <$> decimal
  _     <- string ","
  _     <- (decimal :: Parser Int)
  _     <- string " @@"
  _     <- takeTill isEndOfLine
  return $ HunkInfo line1 line2
