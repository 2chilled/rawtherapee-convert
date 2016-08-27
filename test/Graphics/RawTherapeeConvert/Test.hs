{-# LANGUAGE OverloadedStrings #-}

module Graphics.RawTherapeeConvert.Test where
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, Arbitrary, arbitrary, property, ioProperty, (==>), listOf, elements)
import Test.HUnit (Assertion, assertEqual, assertBool)
import Data.Monoid ((<>), Sum)
import Data.Text (strip, Text, pack, unpack)
import System.FilePath (pathSeparator, takeDirectory, (</>), takeFileName)
import Graphics.RawTherapeeConvert
import Control.Exception (catch, SomeException, throwIO)
import System.Random (randomIO)
import System.Directory (getTemporaryDirectory, doesFileExist, doesDirectoryExist, removeDirectory, createDirectory)
import Data.Foldable (sequenceA_)
import qualified Data.Conduit.Combinators as CC
import Data.Conduit ((=$=), runConduit)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad (when)
import Data.Maybe (isJust, isNothing)

tests :: [Test]
tests = [
  testGroup "Graphics.RawTherapeeConvert" [
    testProperty "getTargetDirectoryPath should return a Left if sourceFilePath is not a subpath of root source dir and a Right otherwise"
      getTargetDirectoryShouldReturnTheCorrectEither
  , testCase "cr2Paths should be a stream emitting cr2 files only"
      cr2PathsShouldBeAStreamEmittingCr2FilesOnly
  , testCase "findPp3 should return the file path to the pp3 if it exists"
      findPp3ShouldReturnTheFilePathToThePp3IfItExists
  , testProperty "isConversionNecessary should return true if target pp3 file is not existing"
      isConversionNecessaryShouldReturnTrueIfTargetPp3FileIsNotExisting
  , testCase "isConversionNecessary should return true if target pp3 file is existing but not equal to given pp3"
      isConversionNecessaryShouldReturnTrueIfTargetPp3FileIsExistingButNotEqualToGivenPp3
  , testCase "isConversionNecessaryShouldReturnFalseIfTargetPp3FileIsExistingAndEqualToGivenPp3"
      isConversionNecessaryShouldReturnFalseIfTargetPp3FileIsExistingAndEqualToGivenPp3
  , testProperty "isConversionNecessary should behave correctly for all cases"
      isConversionNecessaryShouldBehaveCorrectlyForAllCases
    ]
  ]

newtype TestFilePath = TestFilePath FilePath deriving (Show, Eq)

instance Arbitrary TestFilePath where
  arbitrary = TestFilePath <$> (buildPath <$> arbitrary <*> arbitrary)
    where buildPath p1 p2 = let ps = pathSeparator : ""
                                append x = x <> "bla"
                            in ps <> "test" <> append ps <> append p1 <> append ps <> append p2

newtype MyText = MyText Text deriving (Show, Eq)
instance Arbitrary MyText where
  arbitrary =
    let stringGen = listOf $ elements ['a'..'z']
    in MyText . pack <$> stringGen

getTargetDirectoryShouldReturnTheCorrectEither ::
  TestFilePath -> MyText -> Property
getTargetDirectoryShouldReturnTheCorrectEither
  (TestFilePath fp) (MyText anyText) = strip anyText /= "" ==>
    let fpUp = takeDirectory fp
        anyString = unpack anyText
        fpDifferent = fpUp <> "bla"
        dirPath rootSourceDir = getTargetDirectoryPath (RootSourceDir rootSourceDir)
                                                       (RootTargetDir anyString)
                                                       fp
        leftResult = dirPath fpDifferent
        leftResultExpected = Left (SourceFilePathIsNotUnderRootSourceDir (RootSourceDir fpDifferent) fp)
        rightResult = dirPath fpUp
        rightResultExpected = Right (anyString </> takeFileName fp) :: Either GetTargetDirectoryException String
    in property $ leftResult == leftResultExpected
               && rightResult == rightResultExpected

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

newtype SourceFilePathShouldContainPp3 = SourceFilePathShouldContainPp3 Bool deriving (Show, Eq)

instance Arbitrary SourceFilePathShouldContainPp3 where
  arbitrary = SourceFilePathShouldContainPp3 <$> arbitrary

newtype TargetFilePathShouldContainPp3 = TargetFilePathShouldContainPp3 Bool deriving (Show, Eq)

newtype SourcePp3ShouldEqualTargetPp3 = SourcePp3ShouldEqualTargetPp3 Bool deriving (Show, Eq)

instance Arbitrary SourcePp3ShouldEqualTargetPp3 where
  arbitrary = SourcePp3ShouldEqualTargetPp3 <$> arbitrary

instance Arbitrary TargetFilePathShouldContainPp3 where
  arbitrary = TargetFilePathShouldContainPp3 <$> arbitrary

newtype DefaultPp3ShouldBeGiven = DefaultPp3ShouldBeGiven Bool deriving (Show, Eq)

instance Arbitrary DefaultPp3ShouldBeGiven where
  arbitrary = DefaultPp3ShouldBeGiven <$> arbitrary

newtype TargetFilePathShouldExist = TargetFilePathShouldExist Bool deriving (Show, Eq)

instance Arbitrary TargetFilePathShouldExist where
  arbitrary = TargetFilePathShouldExist <$> arbitrary

isConversionNecessaryShouldReturnTrueIfTargetPp3FileIsNotExisting :: SourceFilePathShouldContainPp3
                                                                  -> DefaultPp3ShouldBeGiven
                                                                  -> Property
isConversionNecessaryShouldReturnTrueIfTargetPp3FileIsNotExisting (SourceFilePathShouldContainPp3 sourceFilePathShouldContainPp3)
                                                                  (DefaultPp3ShouldBeGiven defaultPp3ShouldBeGiven) = ioProperty . withEmptyTmpDir $ \dir -> do
  [sourceDir, targetDir] <- let d = [dir </> "source", dir </> "target"] in d <$ createDirectory `traverse` d
  sourceFile <- let f = sourceDir </> "test.CR2" in f <$ writeFile f ""
  _ <- when sourceFilePathShouldContainPp3 (writeFile (sourceDir </> "test.pp3") "")
  defaultPp3 <- if defaultPp3ShouldBeGiven then let f = sourceDir </> "default.pp3" in Just f <$ writeFile f "" else pure Nothing
  isConversionNecessary sourceFile targetDir defaultPp3

isConversionNecessaryShouldReturnTrueIfTargetPp3FileIsExistingButNotEqualToGivenPp3 :: Assertion
isConversionNecessaryShouldReturnTrueIfTargetPp3FileIsExistingButNotEqualToGivenPp3 = withEmptyTmpDir $ \dir -> do
  (sourceDir, targetDir) <- createSourceAndTarget dir
  sourceFile <- let f = sourceDir </> "test.CR2" in f <$ writeFile f ""
  _ <- writeFile (sourceDir </> "test.pp3") "a"
  _ <- writeFile (targetDir </> "test.pp3") "b"
  defaultPp3 <- let f = sourceDir </> "default.pp3" in Just f <$ writeFile f "b"
  result <- isConversionNecessary sourceFile targetDir defaultPp3
  assertBool "" result

isConversionNecessaryShouldReturnFalseIfTargetPp3FileIsExistingAndEqualToGivenPp3 :: Assertion
isConversionNecessaryShouldReturnFalseIfTargetPp3FileIsExistingAndEqualToGivenPp3 = withEmptyTmpDir $ \dir -> do
  (sourceDir, targetDir) <- createSourceAndTarget dir
  sourceFile <- let f = sourceDir </> "test.CR2" in f <$ writeFile f ""
  _ <- writeFile (sourceDir </> "test.pp3") "a"
  _ <- writeFile (targetDir </> "test.pp3") "a"
  _ <- writeFile (targetDir </> "test.jpg") ""
  defaultPp3 <- let f = sourceDir </> "default.pp3" in Just f <$ writeFile f "b"
  result <- isConversionNecessary sourceFile targetDir defaultPp3
  assertBool "" (not result)

createSourceAndTarget :: FilePath -> IO (FilePath, FilePath)
createSourceAndTarget dir =
  let d = [dir </> "source", dir </> "target"]
  in do
    [sourceDir, targetDir] <- d <$ (createDirectory `traverse` d)
    pure (sourceDir, targetDir)

isConversionNecessaryShouldBehaveCorrectlyForAllCases :: SourceFilePathShouldContainPp3
                                                      -> TargetFilePathShouldContainPp3
                                                      -> SourcePp3ShouldEqualTargetPp3
                                                      -> TargetFilePathShouldExist
                                                      -> DefaultPp3ShouldBeGiven
                                                      -> Property
isConversionNecessaryShouldBehaveCorrectlyForAllCases (SourceFilePathShouldContainPp3 sourceFilePathShouldContainPp3)
                                                      (TargetFilePathShouldContainPp3 targetFilePathShouldContainPp3)
                                                      (SourcePp3ShouldEqualTargetPp3 sourcePp3ShouldEqualTargetPp3)
                                                      (TargetFilePathShouldExist targetFilePathShouldExist)
                                                      (DefaultPp3ShouldBeGiven defaultPp3ShouldBeGiven) = ioProperty . withEmptyTmpDir $ \dir ->
  let targetPp3Content = if sourcePp3ShouldEqualTargetPp3 then "a" else "aa"
  in do
    (sourceDir, targetDir) <- createSourceAndTarget dir
    sourceFile <- let f = sourceDir </> "test.CR2" in f <$ writeFile f ""
    _ <- when targetFilePathShouldExist $ writeFile (targetDir </> "test.jpg") ""
    _ <- when sourceFilePathShouldContainPp3 $ writeFile (sourceDir </> "test.jpg.pp3") "a"
    _ <- when targetFilePathShouldContainPp3 $ writeFile (targetDir </> "test.jpg.pp3") targetPp3Content
    defaultPp3 <- if defaultPp3ShouldBeGiven then let f = sourceDir </> "default.pp3" in Just f <$ writeFile f "a" else pure Nothing
    result <- isConversionNecessary sourceFile targetDir defaultPp3
    pure $ result == not (targetFilePathShouldExist && targetFilePathShouldContainPp3 && (sourcePp3ShouldEqualTargetPp3 || (not sourceFilePathShouldContainPp3 && not defaultPp3ShouldBeGiven)))
