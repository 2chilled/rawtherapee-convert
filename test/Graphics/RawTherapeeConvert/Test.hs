module Graphics.RawTherapeeConvert.Test where
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, Arbitrary, arbitrary, property)
import Test.HUnit (Assertion, assertEqual, assertBool)
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
import Data.Maybe (isJust, isNothing)

tests :: [Test]
tests = [
  testGroup "Graphics.RawTherapeeConvert" [
    testProperty "getTargetDirectoryPath should return a left if sourceFilePath is not a subpath of root source dir"
      getTargetDirectoryPathShouldReturnALeftIfSourceFilePathIsNotASubpathOfRootSourceDir
  , testCase "cr2Paths should be a stream emitting cr2 files only"
      cr2PathsShouldBeAStreamEmittingCr2FilesOnly
  , testCase "findPp3 should return the file path to the pp3 if it exists"
      findPp3ShouldReturnTheFilePathToThePp3IfItExists
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

testLogger :: LoggerName
testLogger = LoggerName "testLogger"

type Parent = FilePath
createFile :: Parent -> FilePath -> IO FilePath
createFile p fp = let resultPath = p </> fp
                  in resultPath <$ writeFile resultPath ""

cr2PathsShouldBeAStreamEmittingCr2FilesOnly :: Assertion
cr2PathsShouldBeAStreamEmittingCr2FilesOnly = withEmptyTmpDir $ \dir -> do
  sequenceA_ $ createFile dir <$> ["file1.CR2", "file2.CR2", "any.txt"]
  let toTest = cr2Paths testLogger dir =$= CC.map (const (1 :: Sum Int)) =$= CC.fold
  result <- runResourceT . runConduit $ toTest
  assertEqual "" 2 result

findPp3ShouldReturnTheFilePathToThePp3IfItExists :: Assertion
findPp3ShouldReturnTheFilePathToThePp3IfItExists = withEmptyTmpDir $ \dir -> do
  [f1Cr2, _, f2Cr2] <- sequenceA $ createFile dir <$> ["file1.CR2", "file1.CR2.pp3", "file2.CR2"]
  result1 <- findPp3 f1Cr2
  result2 <- findPp3 f2Cr2
  assertBool "" (isJust result1)
  assertBool "" (isNothing result2)

