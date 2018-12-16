{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DiffLocs.InputLoop where

import qualified Control.Exception    as Exn
import           Data.Attoparsec.Text (parseOnly)
import           Data.Text            (isPrefixOf)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           System.Exit          (exitFailure)
import qualified System.IO            as IO
import qualified System.IO.Error      as IOE
import           Text.Printf          (HPrintfType, hPrintf)

import           DiffLocs.Diffs
import           DiffLocs.Options     (WhichLines (..))

eprintf :: HPrintfType r => String -> r
eprintf = hPrintf IO.stderr

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
  Exn.try (TIO.hGetLine handle) >>= \case
    Left (ioe :: IOE.IOError) -> do
      if IOE.isEOFError ioe then return Nothing else Exn.throwIO ioe
    Right line -> do
      return $ Just line

hTryGetNextLine :: IO.Handle -> Maybe T.Text -> IO (Maybe T.Text)
hTryGetNextLine fileIn prevLine = case prevLine of
  Just _  -> return prevLine
  Nothing -> hTryGetLine fileIn

processHunk :: IO.Handle -> Filename -> HunkInfo -> IO (Maybe T.Text)
processHunk fileIn fileTo hunk = do
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
          printGoodLine fileTo (hunkToLine hunk)
          processHunk fileIn fileTo (succToLine hunk)
        | otherwise -> do
          return $ Just line

processHunks
  :: IO.Handle -> Filename -> Filename -> Maybe T.Text -> IO (Maybe T.Text)
processHunks fileIn fileFrom fileTo prevLine = do
  hTryGetNextLine fileIn prevLine >>= \case
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

run :: IO.Handle -> WhichLines -> Maybe T.Text -> IO ()
run fileIn whichLines prevLine = do
  hTryGetNextLine fileIn prevLine >>= \case
    Nothing -> do
      return ()
    Just line -> do
      case parseOnly fromFile line of
        Left _err -> do
          -- Skip over lines until we find a from line
          run fileIn whichLines Nothing
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
                  run fileIn whichLines nextLine
