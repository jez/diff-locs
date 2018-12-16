{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DiffLocs.InputLoop where

import qualified Control.Exception    as Exn
import           Control.Monad        (when)
import           Data.Attoparsec.Text (parseOnly)
import           Data.Text            (isPrefixOf)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           System.Exit          (exitFailure)
import qualified System.IO            as IO
import qualified System.IO.Error      as IOE
import           Text.Printf          (HPrintfType, hPrintf)

import           DiffLocs.Diffs
import           DiffLocs.Types

eprintf :: HPrintfType r => String -> r
eprintf = hPrintf IO.stderr

shouldPrintRemoved :: WhichLines -> Bool
shouldPrintRemoved LinesAdded           = False
shouldPrintRemoved LinesAddedAndRemoved = True

shouldPrintAdded :: WhichLines -> Bool
shouldPrintAdded LinesAdded           = True
shouldPrintAdded LinesAddedAndRemoved = True

needsPlusOrMinus :: WhichLines -> Bool
needsPlusOrMinus LinesAdded           = False
needsPlusOrMinus LinesAddedAndRemoved = True

printGoodLineRemoved :: WhichLines -> Filename -> FromLine -> IO ()
printGoodLineRemoved whichLines (Filename filename) (FromLine line) = do
  let prefix = if needsPlusOrMinus whichLines then "-" else ""
  TIO.putStr $ prefix <> filename <> ":"
  print line

printGoodLineAdded :: WhichLines -> Filename -> ToLine -> IO ()
printGoodLineAdded whichLines (Filename filename) (ToLine line) = do
  let prefix = if needsPlusOrMinus whichLines then "+" else ""
  TIO.putStr $ prefix <> filename <> ":"
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

noPrevLine :: Maybe T.Text
noPrevLine = Nothing

hTryGetNextLine :: IO.Handle -> Maybe T.Text -> IO (Maybe T.Text)
hTryGetNextLine handle prevLine = case prevLine of
  Just _  -> return prevLine
  Nothing -> hTryGetLine handle

processHunk :: Config -> Filename -> Filename -> HunkInfo -> IO (Maybe T.Text)
processHunk config fileFrom fileTo hunk = do
  let Config fileIn whichLines = config
  hTryGetLine fileIn >>= \case
    Nothing -> do
      return noPrevLine
    Just line -> do
      if
        | " " `isPrefixOf` line -> do
          processHunk config fileFrom fileTo (succBothLines hunk)
        | "-" `isPrefixOf` line -> do
          when (shouldPrintRemoved whichLines) $ do
            printGoodLineRemoved whichLines fileFrom (hunkFromLine hunk)

          processHunk config fileFrom fileTo (succFromLine hunk)
        | "+" `isPrefixOf` line -> do
          when (shouldPrintAdded whichLines) $ do
            printGoodLineAdded whichLines fileTo (hunkToLine hunk)

          processHunk config fileFrom fileTo (succToLine hunk)
        | otherwise -> do
          return $ Just line

processHunks
  :: Config -> Filename -> Filename -> Maybe T.Text -> IO (Maybe T.Text)
processHunks config fileFrom fileTo prevLine = do
  let Config fileIn _whichLines = config
  hTryGetNextLine fileIn prevLine >>= \case
    Nothing -> do
      return noPrevLine
    Just line -> do
      case parseOnly hunkInfo line of
        Left _err -> do
          return $ Just line
        Right hunk -> do
          nextLine <- processHunk config fileFrom fileTo hunk
          processHunks config fileFrom fileTo nextLine

loop :: Config -> Maybe T.Text -> IO ()
loop config prevLine = do
  let Config fileIn _whichLines = config
  hTryGetNextLine fileIn prevLine >>= \case
    Nothing -> do
      return ()
    Just line -> do
      case parseOnly fromFile line of
        Left _err -> do
          -- Skip over lines until we find a from line
          loop config noPrevLine
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
                  nextLine <- processHunks config fileFrom fileTo noPrevLine
                  loop config nextLine

run :: Config -> IO ()
run config = loop config noPrevLine
