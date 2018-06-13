{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Prelude ( error )

-- base --------------------------------

import Control.Exception       ( Exception, SomeException, bracket, fromException )
import Control.Exception.Base  ( IOException )
import Control.Monad           ( Monad, (>>=), join, return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bifunctor          ( first )
import Data.Either             ( Either( Left, Right ), either )
import Data.Eq                 ( Eq, (/=) )
import Data.Function           ( (.), ($) )
import Data.Functor            ( (<$>), fmap )
import Data.List               ( filter, sort )
import Data.Maybe              ( Maybe( Just, Nothing ), fromMaybe, isJust )
import Data.Monoid             ( (<>) )
import Data.String             ( String )
import System.IO               ( FilePath, IO )
import System.IO.Error         ( catchIOError )
import Text.Show               ( Show, show )

-- containers --------------------------

import Data.Map  ( fromList )

-- directory ---------------------------

import System.Directory  ( getTemporaryDirectory, listDirectory
                         , removeDirectoryRecursive, withCurrentDirectory )

-- lens --------------------------------

import Control.Lens.Getter  ( (^.) )
import Control.Lens.Review  ( AReview, re )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError, runExceptT, throwError )

-- path --------------------------------

import qualified  Path

import Path  ( Abs, Dir, Path, PathException, relfile, toFilePath )

-- tasty -------------------------------

import Test.Tasty  ( TestName, TestTree, defaultMain, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( assertBool, assertEqual, testCase )

-- text --------------------------------

import Data.Text     ( lines, unlines )
import Data.Text.IO  ( readFile )

-- unix --------------------------------

import System.Posix.Temp   ( mkdtemp )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FileSplit ( parse, parse', fileSplit )

-------------------------------------------------------------------------------

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
  deriving Eq

instance Show PathError where
  show (PathErr e) = show e

instance Exception PathError

data IOPathError = IOPIOError IOException | IOPPathError PathError
  deriving (Eq, Show)

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
tests =
  let inTempD :: (MonadIO μ, MonadError IOPathError η) => IO α -> μ (η α)
      inTempD =  splitMError. inTempDir . liftIO
      testIO  :: (Show α, Eq α) => TestName -> α -> IO α -> TestTree
      testIO name expect io =
                 testCase name $ inTempD io >>= assertEqual name (Right expect)
      text1  = unlines [ "---- foo"
                       , "great uncle bulgaria"
                       , "madame cholet"
                       , "--------"
                       , "---- bar", "orinoco", "tomsk"
                       , "--------"
                       , "---- quux"
                       , "--------"                       
                       ]

      suffixFailures = let testFail name text = testIO name [] $ do
                             r <- parse' "---- " (Just "--------")
                                         (unlines text)
                             assertBool name (isJust r)
                             listDirectory "."
                        in testGroup "suffixFailures"
                                 [ testFail "file without content"
                                            [ "---- foo" ]
                                 , testFail "files without content"
                                            [ "---- foo", "---- bar" ]
                                 , testFail "file without suffix"
                                            [ "---- foo", "greatunclebulgaria" ]
                                 , testFail "infix file without terminator"
                                            [ "---- foo", "---- bar"
                                            , "--------" ]
                                 , testFail "infix file without name"
                                            [ "---- foo", "--------"
                                            , "--------" ]
                                 ] 
               
   in testGroup "file-split"
            [ testCase "empty parse success" $
                let expect =  fromList []
                 in assertEqual "parse" (Right $ expect) $
                        parse "---- " (Just "--------") ""

            , testCase "parse success" $
                let expect =  fromList [ ([relfile|foo|],
                                            [ "great uncle bulgaria"
                                            , "madame cholet"
                                            ])
                                       , ([relfile|bar|], ["orinoco", "tomsk"])
                                       , ([relfile|quux|], [])
                                       ]
                 in assertEqual "parse" (Right $ expect) $
                        parse "---- " (Just "--------") text1

            , suffixFailures
            , testIO "empty dir" [] (listDirectory ".")
            , testIO "parse'" ["bar", "foo", "quux"] $ do
                r <- parse' "---- " (Just "--------") text1
                assertEqual "split OK" Nothing r
                readFile "bar" >>= assertEqual "file: bar"
                                               (unlines [ "orinoco", "tomsk" ])
                readFile "foo" >>= assertEqual "file: foo"
                                               (unlines [ "great uncle bulgaria"
                                                        , "madame cholet" ])
                readFile "quux" >>= assertEqual "file: quux" ""
                sort <$> listDirectory "."
            , testIO "parse'" ["bar", "foo", "quux"] $ do
                r <- parse' "---- " Nothing
                                    (unlines (filter (/= "--------") $
                                              lines text1))
                assertEqual "split OK" Nothing r
                readFile "bar" >>= assertEqual "file: bar"
                                               (unlines [ "orinoco", "tomsk" ])
                readFile "foo" >>= assertEqual "file: foo"
                                               (unlines [ "great uncle bulgaria"
                                                        , "madame cholet" ])
                readFile "quux" >>= assertEqual "file: quux" ""
                sort <$> listDirectory "."
            ]