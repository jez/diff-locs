{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module DiffLocs.Diffs where

import           Data.Attoparsec.Text
import qualified Data.Text            as T

newtype Filename = Filename T.Text
  deriving (Show, Eq)

newtype FromLine = FromLine Int
  deriving (Show, Eq, Enum)
newtype ToLine = ToLine Int
  deriving (Show, Eq, Enum)

data HunkInfo = HunkInfo
  { hunkFromLine :: FromLine
  , hunkToLine   :: ToLine
  }
  deriving (Show, Eq)


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

