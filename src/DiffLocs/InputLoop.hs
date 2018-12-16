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

-- Our own version of Maybe to be more explicit around why we're passing what's
-- essentially a Maybe T.Text around.
data PrevLine
  = NoPrevLine
  | PrevLine T.Text

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

-- Our newtypes ensure that we always match up FromFile and FromLine
-- when printing.
printGoodLineRemoved :: WhichLines -> FromFile -> FromLine -> IO ()
printGoodLineRemoved whichLines (FromFile filename) (FromLine line) = do
  when (shouldPrintRemoved whichLines) $ do
    let prefix = if needsPlusOrMinus whichLines then "-" else ""
    TIO.putStr $ prefix <> filename <> ":"
    print line

printGoodLineAdded :: WhichLines -> ToFile -> ToLine -> IO ()
printGoodLineAdded whichLines (ToFile filename) (ToLine line) = do
  when (shouldPrintAdded whichLines) $ do
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

-- Catch the isEOFError exception and turn it into a Nothing.
-- We need to handle this case specifically and stop looping.
hTryGetLine :: IO.Handle -> IO (Maybe T.Text)
hTryGetLine handle = do
  Exn.try (TIO.hGetLine handle) >>= \case
    Left (ioe :: IOE.IOError) -> do
      if IOE.isEOFError ioe then return Nothing else Exn.throwIO ioe
    Right line -> do
      return $ Just line

hTryGetNextLine :: IO.Handle -> PrevLine -> IO (Maybe T.Text)
hTryGetNextLine handle prevLine = case prevLine of
  PrevLine line -> return $ Just line
  NoPrevLine    -> hTryGetLine handle

processHunk :: Config -> FromFile -> ToFile -> HunkInfo -> IO PrevLine
processHunk config fileFrom fileTo hunk = do
  let Config fileIn whichLines = config
  hTryGetLine fileIn >>= \case
    Nothing -> do
      return NoPrevLine
    Just line -> do
      if
        | " " `isPrefixOf` line -> do
          processHunk config fileFrom fileTo (succBothLines hunk)
        | "-" `isPrefixOf` line -> do
          printGoodLineRemoved whichLines fileFrom (hunkFromLine hunk)
          processHunk config fileFrom fileTo (succFromLine hunk)
        | "+" `isPrefixOf` line -> do
          printGoodLineAdded whichLines fileTo (hunkToLine hunk)
          processHunk config fileFrom fileTo (succToLine hunk)
        | otherwise -> do
          return $ PrevLine line

processHunks :: Config -> FromFile -> ToFile -> PrevLine -> IO PrevLine
processHunks config fileFrom fileTo prevLine = do
  let Config fileIn _whichLines = config
  hTryGetNextLine fileIn prevLine >>= \case
    Nothing -> do
      return NoPrevLine
    Just line -> do
      case parseOnly hunkInfo line of
        Left _err -> do
          return $ PrevLine line
        Right hunk -> do
          nextLine <- processHunk config fileFrom fileTo hunk
          processHunks config fileFrom fileTo nextLine

loop :: Config -> PrevLine -> IO ()
loop config prevLine = do
  let Config fileIn _whichLines = config
  hTryGetNextLine fileIn prevLine >>= \case
    Nothing -> do
      return ()
    Just line -> do
      case parseOnly fromFile line of
        Left _err -> do
          -- Skip over lines until we find a from line
          loop config NoPrevLine
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
                  nextLine <- processHunks config fileFrom fileTo NoPrevLine
                  loop config nextLine

run :: Config -> IO ()
run config = loop config NoPrevLine
