{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}

module FileSplit
  ( FileOverwrite(..), FileSplitOptions(..), MakeDirs(..), Verbosity(..)
  , parse, parse', fileSplit
  )
where

import Prelude ( otherwise )

-- base --------------------------------

import Control.Monad           ( Monad, (>>), (>>=)
                               , forM_, mapM, join, return, when )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool               ( Bool( False, True ), (&&), bool )
import Data.Either             ( Either( Left , Right ), either )
import Data.Eq                 ( Eq, (==), (/=) )
import Data.Function           ( (.), ($) )
import Data.Functor            ( (<$>) )
import Data.List               ( reverse )
import Data.Maybe              ( Maybe( Just, Nothing ), catMaybes )
import Data.Monoid             ( (<>) )
import System.Exit             ( ExitCode( ExitFailure,ExitSuccess ) )
import System.IO               ( IO, stderr )
import Text.Show               ( show )

-- containers --------------------------

import Data.Map  ( Map
                 , empty, insert, keys, member, traverseWithKey )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- directory ---------------------------

import System.Directory  ( createDirectoryIfMissing )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError, runExceptT, throwError )

-- path --------------------------------

import Path  ( Rel, Dir, File, Path, parseRelFile, parent, reldir, toFilePath )

-- text --------------------------------

import Data.Text     ( Text
                     , drop, isPrefixOf, length, lines, pack, unlines, unpack )
import Data.Text.IO  ( hPutStrLn, writeFile )

-- unix --------------------------------

import System.Posix.Files  ( fileAccess, fileExist )

--------------------------------------------------------------------------------

data FileOverwrite = NoFileOverwrite | FileOverwrite
  deriving Eq
data MakeDirs      = NoMakeDirs | MakeDirs | MakePaths
  deriving Eq
data Verbosity     = NoVerbose | Verbose
  deriving Eq

data FileSplitOptions = FileSplitOptions { verbosity :: Verbosity
                                         , overwrite :: FileOverwrite
                                         , makeDirs  :: MakeDirs
                                         }

instance Default FileSplitOptions where
  def = FileSplitOptions NoVerbose NoFileOverwrite NoMakeDirs

------------------------------------------------------------

type FileName     = Path Rel File
type FileContents = [Text]
type FileMap      = Map FileName FileContents

type Error        = Text
type Errors       = [Error]

------------------------------------------------------------
--                  some error messages                   --
------------------------------------------------------------

quote :: Text -> Text
quote t = "??" <> t <> "??"

pquote :: Text -> Text -> Text
pquote p t = p <> ": " <> quote t

quoteF :: Text -> Path ?? ?? -> Text
quoteF p fn = pquote p (pack $ toFilePath fn)

notAFilePrefixE :: Text -> Text
notAFilePrefixE t   = pquote "not a file prefix" t

dupFileE :: Path Rel File -> Text
dupFileE = quoteF "duplicate filename"

missingSfxE' :: Text -> Path Rel File -> Text -> Text
missingSfxE' t fn u = t <> quoteF "file missing suffix" fn <> ": " <> u

fMissingSfxE :: Path Rel File -> Text
fMissingSfxE fn = missingSfxE' "final " fn ""

missingSfxE :: Path Rel File -> Text -> Text
missingSfxE fn u = missingSfxE' "" fn (" (" <> u <> ")")

noCreateE :: Path Rel Dir -> Text
noCreateE = quoteF "not creating dir"

mfail :: Monad ?? => Text -> Path ?? ?? -> ?? (Maybe Error)
mfail m f = return (Just $ quoteF m f)

cannotWriteDirMME :: Monad ?? => Path ?? Dir -> ?? (Maybe Error)
cannotWriteDirMME = mfail "cannot write to dir"

cannotWriteFileMME :: Monad ?? => Path ?? File -> ?? (Maybe Error)
cannotWriteFileMME = mfail "cannot write to file"

willNotWriteFileMME :: Monad ?? => Path ?? File -> ?? (Maybe Error)
willNotWriteFileMME = mfail "will not write to file"

cannotCreateDirMME :: Monad ?? => Path ?? Dir -> ?? (Maybe Error)
cannotCreateDirMME = mfail "cannot create dir"

cannotCreateFileMME :: Monad ?? => Path ?? File -> ?? (Maybe Error)
cannotCreateFileMME = mfail "cannot create file"

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
parse :: MonadError Errors ?? => Text -> Maybe Text -> Text -> ?? FileMap
parse pfx sfx input =
  either throwError return $ doParse pfx sfx (Nothing, empty, []) (lines input)

------------------------------------------------------------

thisDir :: Path Rel Dir
thisDir = [reldir|./|]

hasParent :: Path Rel ?? -> Bool
hasParent = (/= thisDir) . parent

-- | can we write into this directory (per filesystem)
dirIsWriteable :: Path Rel Dir -> IO (Maybe Error)
dirIsWriteable fn = do
  fexist <- fileExist (toFilePath fn)
  if fexist
  -- fn exists and is writable and executable
  then do access <- fileAccess (toFilePath fn) False True True
          bool (cannotWriteDirMME fn) (return Nothing) access
  else if hasParent fn
       -- fn doesn't exist, parent is ./
       then do access <- fileAccess "." False True True
               bool (cannotCreateDirMME fn) (return Nothing) access
       -- fn doesn't exist, parent isn't ./
       else dirIsWriteable (parent fn)

-- | can we write this filename (per filesystem)
fileIsWriteable :: Bool -> FileName -> IO (Maybe Error)
fileIsWriteable overwr fn = do
  fexist <- fileExist (toFilePath fn)
  if fexist
  -- fn exists and is writable
  then if overwr
       then do access <- fileAccess (toFilePath fn) False True False
               bool (cannotWriteFileMME fn) (return Nothing) access
       else willNotWriteFileMME fn
  else if hasParent fn
       -- fn doesn't exist, is ./
       then do access <- fileAccess "." False True True
               bool (cannotCreateFileMME fn) (return Nothing) access
       -- fn doesn't exist, parent isn't ./
       else dirIsWriteable (parent fn)

filesAreWriteable :: Bool -> [FileName] -> IO Errors
filesAreWriteable overwr fns =
  catMaybes <$> mapM (fileIsWriteable overwr) fns

throwIOErrors :: (MonadIO ??, MonadError [??] ??) => IO [??] -> ?? ()
throwIOErrors ioes = do
  es <- liftIO ioes
  case es of
    [] -> return ()
    _  -> throwError es 

checkDir :: FileSplitOptions -> FileName -> Maybe Error
checkDir opts fn = if hasParent fn
                   then if makeDirs opts == NoMakeDirs
                        then Just $ noCreateE (parent fn)
                        else if hasParent (parent fn)
                             then if makeDirs opts == MakePaths
                                  then Nothing
                                  else Just $ noCreateE (parent fn)
                             else Nothing
                   else Nothing

-- | Take a (Maybe errs), give an Either errs default.
--   
--   for using (Maybe errs) in short-circuiting Either monad, i.e., as soon
--   as you see Just errs, you "exit" at that point

(<!!>) :: MonadError [??] ?? => ?? -> [Maybe ??] -> ?? ??
a <!!> ms = case catMaybes ms of
              [] -> return a
              xs -> throwError xs

-- | split a MonadError out from a monad; that is, takes m ... (which is a
--   monad with an ExceptT constraint) and turns it into a layered m' (m ...)
splitMError :: (MonadError ?? ??, Monad ??) => ExceptT ?? ?? a -> ?? (?? a)
splitMError f = either throwError return <$> runExceptT f

-- | parse incoming text, then check the results (if not errors) against the
--   applicable options
parseCheck :: MonadError Errors ?? =>
              FileSplitOptions -> Text -> Maybe Text -> Text -> ?? FileMap
parseCheck opts prefix suffix input = do
  filemap <- parse prefix suffix input
  filemap <!!> (checkDir opts <$> keys filemap)
  
writeText :: MonadIO ?? => FileSplitOptions -> FileName -> FileContents -> ?? ()
writeText opts fn ls = liftIO $ do
  when (hasParent fn && makeDirs opts /= NoMakeDirs) $
    createDirectoryIfMissing (makeDirs opts == MakePaths)
                             (toFilePath $ parent fn)
  when (verbosity opts == Verbose) (hPutStrLn stderr $ quoteF "writing file" fn)
  writeFile (toFilePath fn) (unlines ls)

writeTexts :: MonadIO ?? => FileSplitOptions -> FileMap -> ?? ()
writeTexts opts map = traverseWithKey (writeText opts) map >> return ()

-- | parse input text in light of prefix & suffix, either writing the files
--   or returning any errors.  Checks for file overwrites, directories/paths
--   that need to be created also.
parse' :: (MonadIO ??, MonadError Errors ??) =>
          FileSplitOptions -> Text -> Maybe Text -> Text -> ?? ()
parse' opts prefix suffix input = do
  filemap <- join . return $ parseCheck opts prefix suffix input
  () <- throwIOErrors (filesAreWriteable (overwrite opts == FileOverwrite)
                                         (keys filemap))
  writeTexts opts filemap

-- | parse input text in light of prefix & suffix, either writing the files
--   or warning of errors, then exiting
fileSplit :: FileSplitOptions -> Text -> Maybe Text -> Text -> IO ExitCode
fileSplit opts prefix suffix input = do
  splitMError (parse' opts prefix suffix input) >>= \case
    Right ()  -> return ExitSuccess
    Left errs -> forM_ errs (hPutStrLn stderr) >> return (ExitFailure 3)

-- that's all, folks! ----------------------------------------------------------
