{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main where

import qualified Control.Exception    as Exn
import           Data.Attoparsec.Text
import           Data.Text            (isPrefixOf)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           System.Exit          (exitFailure)
import qualified System.IO            as IO
import qualified System.IO.Error      as IOE
import           Text.Printf          (HPrintfType, hPrintf)

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

eprintf :: HPrintfType r => String -> r
eprintf = hPrintf IO.stderr

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
  _     <- decimal
  _     <- string " +"
  line2 <- ToLine <$> decimal
  _     <- string ","
  _     <- decimal
  _     <- string " @@"
  _     <- takeTill isEndOfLine
  return $ HunkInfo line1 line2

printGoodLine :: Filename -> ToLine -> IO ()
printGoodLine (Filename filename) (ToLine line) = do
  TIO.putStr $ filename <> ":"
  print line

succFromLine :: HunkInfo -> HunkInfo
succFromLine (HunkInfo { hunkFromLine, hunkToLine }) =
  HunkInfo (succ hunkFromLine) hunkToLine

succToLine :: HunkInfo -> HunkInfo
succToLine (HunkInfo { hunkFromLine, hunkToLine }) =
  HunkInfo hunkFromLine (succ hunkToLine)

succBothLines :: HunkInfo -> HunkInfo
succBothLines (HunkInfo { hunkFromLine, hunkToLine }) =
  HunkInfo (succ hunkFromLine) (succ hunkToLine)

hTryGetLine :: IO.Handle -> IO (Maybe T.Text)
hTryGetLine handle = do
  Exn.try (TIO.hGetLine fileIn) >>= \case
    Left (ioe :: IOE.IOError) -> do
      if IOE.isEOFError ioe then return Nothing else Exn.throwIO ioe
    Right line -> do
      return $ Just line

processHunk :: IO.Handle -> Filename -> HunkInfo -> IO (Maybe T.Text)
processHunk fileIn fileTo hunk lineOffset = do
  hTryGetLine fileIn >>= \case
    Nothing -> do
      return Nothing
    Just line -> do
      if
        | " " `isPrefixOf` line -> do
          processHunk fileIn fileTo (succBothLines hunk)
        | "-" `isPrefixOf` line -> do
          processHunk fileIn fileTo (succFromLine hunk)
        | "+" `isPrefixOf` line -> do
          printGoodLine fileTo (hunkFromLine hunk)
          processHunk fileIn fileTo (succToLine hunk)
        | otherwise -> do
          return $ Just line

processHunks
  :: IO.Handle -> Filename -> Filename -> Maybe T.Text -> IO (Maybe T.Text)
processHunks fileIn fileFrom fileTo currentLine = do
  firstLine <- case currentLine of
    Nothing -> hTryGetLine fileIn
    Just ln -> return $ Just ln
  case firstLine of
    Nothing -> do
      return Nothing
    Just line -> do
      case parseOnly hunkInfo line of
        Left _err -> do
          -- Read a line, but didn't parse as a hunk. Return so that
          return $ Just line
        Right hunk -> do
          nextLine <- processHunk fileIn fileTo hunk
          processHunks fileIn fileFrom fileTo nextLine

run :: IO.Handle -> Maybe T.Text -> IO ()
run fileIn currentLine = do
  firstLine <- case currentLine of
    Nothing -> hTryGetLine fileIn
    Just ln -> return $ Right ln
  case firstLine of
    Nothing -> do
      return ()
    Just line -> do
      case parseOnly fromFile line of
        Left _err -> do
          -- Skip over lines until we find a from line
          run fileIn Nothing
        Right fileFrom -> do
          hTryGetLine fileIn >>= \case
            Nothing -> do
              return ()
            Just secondLine -> do
              case parseOnly toFile secondLine of
                Left err -> do
                  -- Line immediately after fromFile must be toFile
                  eprintf "Error parsing toFile: %s\n" err
                  exitFailure
                Right fileTo -> do
                  nextLine <- processHunks fileIn fileFrom fileTo Nothing
                  run fileIn nextLine

main :: IO ()
main = do
  -- TODO(jez) Open either file or stdin
  run IO.stdin Nothing
