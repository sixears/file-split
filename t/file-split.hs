{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude ( error )

-- base --------------------------------

import Control.Exception       ( Exception, SomeException, bracket, fromException )
import Control.Exception.Base  ( IOException )
import Control.Monad           ( Monad, (>>=), join, return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bifunctor          ( first )
import Data.Either             ( Either( Left, Right ), either )
import Data.Function           ( (.), ($) )
import Data.Functor            ( (<$>), fmap )
import Data.Maybe              ( fromMaybe )
import Data.Monoid             ( (<>) )
import Data.String             ( String )
import System.IO               ( FilePath, IO )
import System.IO.Error         ( catchIOError )
import Text.Show               ( Show, show )

-- directory ---------------------------

import System.Directory  ( getTemporaryDirectory, removeDirectoryRecursive
                         , withCurrentDirectory )

-- lens --------------------------------

import Control.Lens.Getter  ( (^.) )
import Control.Lens.Review  ( AReview, re )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError, runExceptT, throwError )

-- path --------------------------------

import qualified  Path

import Path  ( Abs, Dir, Path, PathException, toFilePath )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- text --------------------------------

import Data.Text  ( unlines )

-- unix --------------------------------

import System.Posix.Temp   ( mkdtemp )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

-- import qualified  Text.T.Fmt  as  Fmt

-------------------------------------------------------------------------------

text1 = unlines [ "---- foo", "my test text", "--------" ]

----------------------------------------

ioMonadError :: (MonadIO μ, MonadError IOException η) => IO α -> μ (η α)
ioMonadError io = liftIO $ catchIOError (return <$> io) (return . throwError)

-- | fromRight, throwing an error on Left
fromRight :: MonadError e m => Either e a -> m a
fromRight = either throwError return

-- | map the exception part of a MonadError; can also be used on MonadThrow
--   to convert to a MonadError (or indeed any Either)
mapMError :: (MonadError β η) => (α -> β) -> Either α x -> η x
mapMError f = fromRight . first f

data PathError = PathErr { unPathErr :: PathException }

instance Show PathError where
  show (PathErr e) = show e

instance Exception PathError

data IOPathError = IOPIOError IOException | IOPPathError PathError

toIOPathError :: MonadError IOPathError η =>
                 Either IOException (Either PathError α) -> η α
toIOPathError a = case a of
                    Left ie          -> throwError $ IOPIOError   ie
                    Right (Left pe)  -> throwError $ IOPPathError pe
                    Right (Right a') -> return a'

----------------------------------------

-- | split a MonadError out from a monad; that is, takes m ... (which is a
--   monad with an ExceptT constraint) and turns it into a layered m' (m ...)
splitMError :: (MonadError ε η, Monad μ) => ExceptT ε μ a -> μ (η a)
splitMError f = either throwError return <$> runExceptT f



-- | Easy fn to make classy values for options using prisms.
--   Given a lens x (presumed to be a Prism', where the first type value might
--   well be constrained unto a class), and a value y,
--   construct a value y of type suitable to the prism.
--
--   It maybe be useful to recall that AReview is a generalization of Prism',
--   so (##) matches the slightly stricter typing:
--
--   @
--     (##) :: Prism' t s -> s -> t
--   @

(##) :: AReview t s -> s -> t
x ## y = y ^. re x

-- pathError :: (AsPathError e, Printable τ) => Text -> τ -> SomeException -> e
pathError :: String -> String -> SomeException -> PathError
pathError funcname fn e = PathErr $ fromMaybe err (fromException e)
  where err = error $ "PathError '" <> show e <> "' from " <> funcname
                   <> " (" <> fn <> ")"

parseAbsDir :: MonadError PathError μ => String -> μ (Path Abs Dir)
parseAbsDir fn = mapMError (pathError "parseAbsDir" fn) $ Path.parseAbsDir fn

-- | given a function name, a filename and an exception, pull the
--   PathException out from the SomeException; or else create a suitable
--   error for a pseudo-PathException (this should never happen, because
--   parse{Abs,Rel}{File,Dir} should never return a SomeException wrapped
--   around anything other than a PathException)
getTempDir :: (MonadIO μ, MonadError IOPathError η)=> μ (η (Path Abs Dir))
getTempDir = let go = ioMonadError getTemporaryDirectory >>= (parseAbsDir <$>)
              in toIOPathError <$> splitMError go

{-
-- | create a unique temporary directory under TMPDIR, and return its name
mkTempDir :: (MonadIO μ, AsPathError ε, AsIOError ε, MonadError ε μ) =>
             μ (Path Abs Dir)
mkTempDir = do
  tmp    <- getTempDir
  case tmp of
    Left iop -> throwError iop
    Right t  -> return $  mkdtemp (toFilePath tmp)
  tmpDir <- asIOError $
  parseAbsDir tmpDir
-}

mkTempSubDir :: (MonadIO m, MonadError IOPathError m) =>
                Path Abs Dir -> m (Path Abs Dir)
mkTempSubDir =
  let parseabsdir :: MonadError IOPathError η => String -> η (Path Abs Dir)
      parseabsdir =  mapMError IOPPathError . parseAbsDir
      mktempd   :: (MonadIO μ, MonadError IOException η) =>
                   Path Abs Dir -> μ (η FilePath)
      mktempd   =  ioMonadError . mkdtemp . toFilePath
      mktempd'  :: (MonadIO μ, MonadError IOPathError η) =>
                   Path Abs Dir -> μ (η FilePath)
      mktempd'  =  fmap (mapMError IOPIOError) . mktempd
   in join . fmap (join . fmap parseabsdir) . mktempd'

mkTempDir :: (MonadError IOPathError m, MonadIO m) => m (Path Abs Dir)
mkTempDir = do
  tmp <- getTempDir
  case tmp of
    Left iop -> throwError iop
    Right t  -> mkTempSubDir t

rmdirRecursive :: (MonadIO μ, MonadError IOPathError μ) => Path β Dir -> μ ()
rmdirRecursive = join . fmap (mapMError IOPIOError)
               . ioMonadError . removeDirectoryRecursive . toFilePath

-- | Perform some IO within a temporary directory freshly created by `mkTempDir`.
--   Cleans away the created directory when IO is complete.

inTempDir :: (MonadIO μ, MonadError IOPathError μ) =>
             (ExceptT IOPathError IO α) -> μ α
inTempDir io = do
  let make  = splitMError mkTempDir
      break :: Either IOPathError (Path Abs Dir) -> IO (Either IOPathError ())
      break = either (return . throwError) (splitMError . rmdirRecursive)
      doIndir go d = withCurrentDirectory (toFilePath d) (splitMError go)
  join . liftIO $ bracket make break (either (return . throwError) (doIndir io))

------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "file-split" [  ]
