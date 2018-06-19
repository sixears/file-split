{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import Prelude ( )

-- base --------------------------------

import Control.Applicative     ( (<**>), (<*>), optional )
import Control.Monad           ( (>>=) )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Eq                 ( (==) )
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
                                    , failureCode, flag, fullDesc, help, info
                                    , prefs, progDesc, short, showHelpOnEmpty
                                    , showHelpOnError, strArgument
                                    )
import Options.Applicative.Extra    ( customExecParser, helper )
import Options.Applicative.Types    ( Parser, ParserPrefs )

-- text --------------------------------

import Data.Text     ( Text, unpack )
import Data.Text.IO  ( getContents )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FileSplit ( FileOverwrite( FileOverwrite, NoFileOverwrite )
                 , FileSplitOptions( FileSplitOptions )
                 , MakeDirs( NoMakeDirs, MakeDirs, MakePaths )
                 , fileSplit
                 )

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

data Options = Options { _splitPfx  :: Text
                       , _splitSfx  :: Maybe Text
                       , _overwrite :: FileOverwrite
                       , _makeDirs  :: MakeDirs
                       , _makePaths :: MakeDirs
                       }

$( makeLenses ''Options )

-- add -O to overwrite extant files
-- add -m to make necessary dirs (one level)
-- add -M to make necessary dirs (multiple levels)

options :: Parser Options
options =  let prefixHelp = help "prefix for filename strings"
               suffixHelp = help "line format for end-of-file line"
               mHelp = help "auto-create any directories (up to a depth of 1)"
               pHelp = help "auto-create any directory paths"
               oHelp = help "overwrite any extant files"
            in (Options <$> textArgument prefixHelp
                        <*> optional (textArgument suffixHelp)
                        <*> flag NoFileOverwrite FileOverwrite
                                                      (short 'O' <> oHelp)
                        <*> flag NoMakeDirs MakeDirs  (short 'm' <> mHelp)
                        <*> flag NoMakeDirs MakePaths (short 'p' <> pHelp)
               )

------------------------------------------------------------

main :: IO ()
main = do
  opts <- optParser "write files per stdin instructions" options
  let mkpaths = if opts ^. makePaths == NoMakeDirs
                then opts ^. makeDirs
                else opts ^. makePaths
  getContents >>= fileSplit (FileSplitOptions (opts ^. overwrite) mkpaths)
                            (opts ^. splitPfx) (opts ^. splitSfx) >>= exitWith

-- that's all, folks! ----------------------------------------------------------
