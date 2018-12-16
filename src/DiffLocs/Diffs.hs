{-# LANGUAGE OverloadedStrings #-}

module DiffLocs.Diffs where

import           Data.Attoparsec.Text

import           DiffLocs.Types

fromFile :: Parser Filename
fromFile = do
  _        <- string "--- "
  _        <- option "" (string "a/")
  filename <- takeTill isEndOfLine
  return $ Filename filename

toFile :: Parser Filename
toFile = do
  _        <- string "+++ "
  _        <- option "" (string "b/")
  filename <- takeTill isEndOfLine
  return $ Filename filename

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
