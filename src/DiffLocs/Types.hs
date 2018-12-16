{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DiffLocs.Types where

import qualified Data.Text as T
import qualified System.IO as IO

data Input
  = InputFromStdin
  | InputFromFile FilePath

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

newtype FromFile = FromFile T.Text
  deriving (Show, Eq)
newtype ToFile = ToFile T.Text
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

