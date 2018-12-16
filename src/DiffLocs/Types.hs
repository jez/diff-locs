{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DiffLocs.Types where

import qualified Data.Text as T
import qualified System.IO as IO

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

data Config = Config
  { configFileIn     :: IO.Handle
  , configWhichLines :: WhichLines
  }

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

