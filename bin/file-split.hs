{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import Prelude ( )

-- base --------------------------------

import Control.Applicative     ( (<**>), (<*>), optional )
import Control.Monad           ( (>>=) )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Function           ( (.), ($) )
import Data.Functor            ( (<$>) )
import Data.Maybe              ( Maybe )
import Data.Monoid             ( (<>) )
import System.Exit             ( exitWith )
import System.IO               ( IO )

-- lens --------------------------------

import Control.Lens.Getter  ( (^.) )
import Control.Lens.TH      ( makeLenses )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( ArgumentFields, Mod
                                    , failureCode, fullDesc, help, info, prefs
                                    , progDesc, showHelpOnEmpty, showHelpOnError
                                    , strArgument
                                    )
import Options.Applicative.Extra    ( customExecParser, helper )
import Options.Applicative.Types    ( Parser, ParserPrefs )

-- text --------------------------------

import Data.Text     ( Text, unpack )
import Data.Text.IO  ( getContents )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FileSplit ( fileSplit )

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

------------------------------------------------------------

data Options = Options { _splitPfx :: Text, _splitSfx :: Maybe Text }

$( makeLenses ''Options )

-- add -O to overwrite extant files
-- add -m to make necessary dirs (one level)
-- add -M to make necessary dirs (multiple levels)

options :: Parser Options
options =  let prefixHelp = help "prefix for filename strings"
               suffixHelp = help "line format for end-of-file line"
            in (Options <$> textArgument prefixHelp
                        <*> optional (textArgument suffixHelp))

------------------------------------------------------------

main :: IO ()
main = do
  opts <- optParser "write files per stdin instructions" options
  getContents >>= fileSplit (opts ^. splitPfx) (opts ^. splitSfx) >>= exitWith

-- that's all, folks! ----------------------------------------------------------
