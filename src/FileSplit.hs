{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module FileSplit
  ( parse, parse', fileSplit
  )
where

import Prelude ( otherwise )

-- base --------------------------------

import Control.Monad           ( (>>), (>>=), forM_, return )
import Data.Either             ( Either( Left , Right ) )
import Data.Eq                 ( (==) )
import Data.Function           ( ($) )
import Data.List               ( reverse )
import Data.Maybe              ( Maybe( Just, Nothing ) )
import Data.Monoid             ( (<>) )
import System.Exit             ( ExitCode( ExitFailure,ExitSuccess ) )
import System.IO               ( IO , stderr )
import Text.Show               ( show )

-- containers --------------------------

import Data.Map  ( Map, empty, insert, member, toList )

-- path --------------------------------

import Path  ( Rel, File, Path, parseRelFile, toFilePath )

-- text --------------------------------

import Data.Text     ( Text
                     , drop, isPrefixOf, length, lines, pack, unlines, unpack )
import Data.Text.IO  ( hPutStrLn, writeFile )

--------------------------------------------------------------------------------

type FileName     = Path Rel File
type FileContents = [Text]
type FileMap      = Map FileName FileContents

type Error        = Text
type Errors       = [Error]

------------------------------------------------------------

notAFilePrefixE :: Text -> Text
notAFilePrefixE t   = "not a file prefix: «" <> t <> "»"

dupFileE :: Path Rel File -> Text
dupFileE fn         = "duplicate filename «" <> pack (show fn) <> "»"

missingSfxE' :: Text -> Path Rel File -> Text -> Text
missingSfxE' t fn u = t <> "file missing suffix: "
                   <> "«" <> pack (show fn) <> "»"
                   <> u

fMissingSfxE :: Path Rel File -> Text
fMissingSfxE fn     = missingSfxE' "final " fn ""

missingSfxE :: Path Rel File -> Text -> Text
missingSfxE fn u    = missingSfxE' "" fn (" (" <> u <> ")")


----------------------------------------
        
type FileParse = (Maybe (FileName, [Text]), FileMap, Errors)

addErr :: Error -> Text -> Maybe Text -> Maybe (FileName, [Text]) -> FileMap
       -> [Error] -> [Text] -> Either Errors FileMap
addErr e pfx sfx inflight filemap es ts =
  doParse pfx sfx (inflight, filemap, e:es) ts

----------------------------------------

newFile :: Text -> Maybe Text -> Text -> [Text] -> FileMap -> [Error]
        -> Either Errors FileMap
newFile pfx sfx t ts filemap es =
  case parseRelFile (unpack $ drop (length pfx) t) of
    Left  e  -> addErr (pack (show e)) pfx sfx Nothing filemap es ts
    Right fn -> doParse pfx sfx (Just (fn,[]), filemap, es) ts

----------------------------------------

doneFile :: Text -> Maybe Text -> FileName -> FileContents -> FileMap -> Errors
         -> [Text] -> Either Errors FileMap
doneFile pfx sfx fn ls filemap es ts =
  if fn `member` filemap
  then addErr (dupFileE fn) pfx sfx Nothing filemap es ts
  else doParse pfx sfx (Nothing, insert fn (reverse ls) filemap, es) ts

----------------------------------------

missingSfx :: Text -> Maybe Text -> FileName -> Errors -> FileMap -> Errors
           -> Text -> [Text] -> Either Errors FileMap
missingSfx pfx sfx fn ls filemap es t ts =
  doneFile pfx sfx fn ls filemap (missingSfxE fn t : es) ts

----------------------------------------

doParse :: Text -> Maybe Text -> FileParse -> FileContents
        -> Either Errors FileMap

-- all done
doParse _   _          (Nothing     , filemap, []) [] = Right filemap
doParse _   _          (Nothing     , _,       es) [] = Left  es

-- all done (no suffix)
doParse pfx Nothing    (Just (fn,ls), filemap, es) [] =
  doneFile pfx Nothing fn ls filemap es []

-- no more text, but expecting terminating suffix
doParse pfx (Just sfx) (Just (fn,ls), filemap, es) [] =
  doneFile pfx (Just sfx) fn ls filemap (fMissingSfxE fn : es) []

doParse pfx sfx        (Nothing     , filemap, es) (t : ts)
                       | pfx `isPrefixOf` t = newFile pfx sfx t ts filemap es
                       | otherwise          = doParse pfx sfx
                                                    (Nothing, filemap,
                                                     notAFilePrefixE t : es)
                                                    ts

doParse pfx sfx        (Just (fn,ls), filemap, es) (t : ts)
                       | (Just t) == sfx    = doneFile pfx sfx fn
                                                       ls filemap es ts
                       | pfx `isPrefixOf` t = case sfx of
                                                Just _  -> missingSfx pfx sfx fn
                                                                      ls
                                                                      filemap
                                                                      es t ts
                                                Nothing -> doneFile pfx sfx fn
                                                                    ls filemap
                                                                    es (t : ts)
                       | otherwise          = doParse pfx sfx
                                                    (Just (fn,t:ls), filemap,es)
                                                    ts

----------------------------------------

-- | parse input text in light of prefix & suffix, returning either errors
--   or a list of files with their contents
parse :: Text -> Maybe Text -> Text -> Either Errors FileMap
parse pfx sfx input = doParse pfx sfx (Nothing, empty, []) (lines input)

------------------------------------------------------------

-- | parse input text in light of prefix & suffix, either writing the files
--   or returning any errors
parse' :: Text -> Maybe Text -> Text -> IO (Maybe Errors)
parse' prefix suffix input =
  -- XXX support for overwrite or not of existing files
  -- XXX support for auto-creation or not of directories
  case parse prefix suffix input of
    Left  errs  -> return $ Just errs
    Right files -> do forM_ (toList files)
                            (\(fn,ls) -> writeFile (toFilePath fn) (unlines ls))
                      return Nothing

-- | parse input text in light of prefix & suffix, either writing the files
--   or warning of errors, then exiting
fileSplit :: Text -> Maybe Text -> Text -> IO ExitCode
fileSplit prefix suffix input = do
  parse' prefix suffix input >>= \case
    Nothing   -> return ExitSuccess
    Just errs -> forM_ errs (hPutStrLn stderr) >> return (ExitFailure 3)

-- that's all, folks! ----------------------------------------------------------
