#! /usr/bin/env nix-shell
#! -*- mode: haskell -*-
#! nix-shell -i runghc -p myHaskellEnv

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import Prelude ( error, otherwise )

-- base --------------------------------

import Control.Applicative     ( (<**>), (<*>) )
import Control.Monad           ( forM_ )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bifunctor          ( first )
import Data.Eq                 ( (==) )
import Data.Function           ( (.), ($) )
import Data.Functor            ( (<$>) )
import Data.List               ( filter, last, null )
import Data.Maybe              ( Maybe( Just, Nothing ), catMaybes )
import Data.Monoid             ( (<>) )
import Data.String             ( String )
import System.Exit             ( ExitCode( ExitFailure,ExitSuccess ), exitWith )
import System.IO               ( IO, stderr )

-- data-textual ------------------------

import Data.Textual  ( Printable, Textual, fromString, toString )

-- lens --------------------------------

import Control.Lens.Getter  ( (^.) )
import Control.Lens.TH      ( makeLenses )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( ArgumentFields, Mod
                                    , command, failureCode, flag, fullDesc, help
                                    , info, long, prefs, progDesc, short
                                    , showHelpOnEmpty, showHelpOnError
                                    , strArgument
                                    )
import Options.Applicative.Extra    ( customExecParser, helper, hsubparser )
import Options.Applicative.Types    ( Parser, ParserInfo, ParserPrefs )

-- split -------------------------------

import Data.List.Split  ( dropInitBlank, split, whenElt )

-- text --------------------------------

import Data.Text     ( Text, drop, isPrefixOf, length, lines, unpack )
import Data.Text.IO  ( getContents, hPutStrLn, putStrLn )

--------------------------------------------------------------------------------

-- | standard parser preferences
parserPrefs :: ParserPrefs
parserPrefs = prefs $ showHelpOnError <> showHelpOnEmpty

----------------------------------------

optParser :: (MonadIO μ) => Text     -- prog description
                         -> Parser α -- options parser
                         -> μ α
optParser t i =
  let infoMod = fullDesc <> progDesc (unpack t) <> failureCode 2
   in liftIO . customExecParser parserPrefs $ info (i <**> helper) infoMod

----------------------------------------

textArgument :: Mod ArgumentFields Text -> Parser Text
textArgument = strArgument

----------------------------------------

pairs :: Printable α => [[α]] -> [(α,[α])]
pairs ([]    : _ )      = error "empty file prefix"
pairs ([x]   : y : xs') = (x,y) : pairs xs'
pairs ([z]   : [])      = error $ "file without content: '" <> toString z <> "'"
pairs ((x:_) : _ )      = error $ "non-file prefix: '" <> toString x <> "'"
pairs []                = []

----------------------------------------

check :: Text -> (Text, [Text]) -> Maybe Text
check _ (fn,[]) = Just $ "file '" <> fn <> "' missing content"
check s (fn,xs) | last xs == s = Nothing
                | otherwise    = Just $ "file '" <> fn <> "' missing terminator <<"  <> last xs <> ">>"

------------------------------------------------------------

data Options = Options { _splitPfx :: Text, _splitSfx :: Text }

$( makeLenses ''Options )

options =  (Options <$> textArgument (help "prefix for filename strings")
                    <*> textArgument (help "line format for end-of-file line"))

------------------------------------------------------------

main :: IO ()
main = do
  opts <- optParser "write files per stdin instructions" options
  t <- getContents
  let ts        = lines t
      split_pfx = opts ^. splitPfx
      slen      = length split_pfx
      mySplit p = split (dropInitBlank $ whenElt p)
      fs        = let ps = pairs $ mySplit (split_pfx `isPrefixOf`) ts
                   in first (drop slen) <$> ps
      es        = catMaybes $ (check (opts ^. splitSfx)) <$> fs
  if null es
  then do forM_ fs (\(fn,ls) -> putStrLn fn)
          exitWith ExitSuccess
  else do forM_ es $ hPutStrLn stderr
          exitWith (ExitFailure 3)

-- that's all, folks! ----------------------------------------------------------
