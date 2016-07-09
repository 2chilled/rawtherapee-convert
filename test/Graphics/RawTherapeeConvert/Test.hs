module Graphics.RawTherapeeConvert.Test where
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, Arbitrary, arbitrary, property)
import Test.HUnit (Assertion, assertEqual)
import Data.Monoid ((<>), Sum)
import System.FilePath (pathSeparator, takeDirectory, (</>))
import Graphics.RawTherapeeConvert
import Control.Exception (catch, SomeException, throwIO)
import System.Random (randomIO)
import System.Directory (getTemporaryDirectory, doesFileExist, doesDirectoryExist, removeDirectory, createDirectory)
import Data.Foldable (sequenceA_)
import qualified Data.Conduit.Combinators as CC
import Data.Conduit ((=$=), runConduit)
import Control.Monad.Trans.Resource (runResourceT)

tests :: [Test]
tests = [
  testGroup "Graphics.RawTherapeeConvert" [
    testProperty "getTargetDirectoryPath should return a left if sourceFilePath is not a subpath of root source dir"
      getTargetDirectoryPathShouldReturnALeftIfSourceFilePathIsNotASubpathOfRootSourceDir
  , testCase "cr2Paths should be a stream emitting cr2 files only"
      cr2PathsShouldBeAStreamEmittingCr2FilesOnly
    ]
  ]

newtype TestFilePath = TestFilePath FilePath deriving (Show, Eq)

instance Arbitrary TestFilePath where
  arbitrary = TestFilePath <$> (buildPath <$> arbitrary <*> arbitrary)
    where buildPath p1 p2 = let ps = pathSeparator : ""
                            in ps <> "test" <> ps <> p1 <> ps <> p2

getTargetDirectoryPathShouldReturnALeftIfSourceFilePathIsNotASubpathOfRootSourceDir ::
  TestFilePath -> String -> Property
getTargetDirectoryPathShouldReturnALeftIfSourceFilePathIsNotASubpathOfRootSourceDir
  (TestFilePath fp) anyString = let fpUp = takeDirectory fp
                                    result = getTargetDirectoryPath (RootSourceDir fpUp)
                                                                    (RootTargetDir anyString)
                                                                    fp
                                in property $ result == Left SourceFilePathIsNotUnderRootSourceDir

withEmptyTmpDir :: (FilePath -> IO x) -> IO x
withEmptyTmpDir f = do
  tmpDir <- getEmptyTemporaryDirectory
  f tmpDir `catch` handler tmpDir

  where handler :: FilePath -> SomeException -> IO x
        handler fp e = do
          removeDirectory fp `catch` (handleAll . const $ return ())
          throwIO e

        handleAll :: (SomeException -> IO x) -> SomeException -> IO x
        handleAll f' = f'

        getEmptyTemporaryDirectory :: IO FilePath
        getEmptyTemporaryDirectory = do
          randomTempDir <- (</>) <$> getTemporaryDirectory <*> randomString
          existingFileOrDir' <- existingFileOrDir randomTempDir
          emptyDir <- if existingFileOrDir' then getEmptyTemporaryDirectory
                      else return randomTempDir
          createDirectory emptyDir
          return emptyDir

        existingFileOrDir :: FilePath -> IO Bool
        existingFileOrDir fp = (||) <$> doesFileExist fp <*> doesDirectoryExist fp

        randomString :: IO String
        randomString = show <$> (randomIO :: IO Int)

cr2PathsShouldBeAStreamEmittingCr2FilesOnly :: Assertion
cr2PathsShouldBeAStreamEmittingCr2FilesOnly = withEmptyTmpDir $ \dir ->
  let createFile = flip writeFile "" . (dir </>)
  in do
    sequenceA_ $ createFile <$> ["file1.CR2", "file2.CR2", "any.txt"]
    let toTest = cr2Paths dir =$= CC.map (const (1 :: Sum Int)) =$= CC.fold
    result <- runResourceT . runConduit $ toTest
    assertEqual "" 2 result
