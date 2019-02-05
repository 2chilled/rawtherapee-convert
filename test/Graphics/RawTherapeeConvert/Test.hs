{-# LANGUAGE OverloadedStrings #-}

module Graphics.RawTherapeeConvert.Test where

import Data.Ini (readIniFile)
import Test.Hspec
import Test.QuickCheck (Property, Arbitrary, arbitrary, property, ioProperty, (==>), listOf, elements)
import Data.Monoid ((<>), Sum)
import Data.Text (strip, Text, pack, unpack)
import System.FilePath (pathSeparator, takeDirectory, (</>))
import Graphics.RawTherapeeConvert
import Control.Exception (catch, SomeException, throwIO)
import System.Random (randomIO)
import System.Directory (getTemporaryDirectory, doesFileExist, doesDirectoryExist, removeDirectory, createDirectory)
import Data.Foldable (sequenceA_)
import qualified Data.Conduit.Combinators as CC
import Data.Conduit ((.|), runConduit)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad (when, (>=>))
import Data.Maybe (isJust, isNothing)
import Debug.Trace (trace)

tests :: SpecWith ()
tests =
  describe "Graphics.RawTherapeeConvert" $ do
    describe "getTargetDirectoryPath" $ do
      it "should return a Left if sourceFilePath is not a subpath of root source dir and a Right otherwise" $
        property getTargetDirectoryShouldReturnTheCorrectEither
      it "should work correctly" $
          let fp = "/bla/1/2/blubb.CR2"
              targetDir = "/aha/x/"
              result = getTargetDirectoryPath (RootSourceDir "/bla/")
                                              (RootTargetDir targetDir)
                                              fp
              resultExpected = Right "/aha/x/1/2" :: Either GetTargetDirectoryException String
          in result `shouldBe` resultExpected
    describe "cr2Paths" $
      it "should be a stream emitting cr2 files only" $
        withEmptyTmpDir $ \dir -> do
          sequenceA_ $ createFile dir <$> ["file1.CR2", "file2.CR2", "any.txt"]
          let toTest = cr2Paths testLogger dir .| CC.map (const (1 :: Sum Int)) .| CC.fold
          result <- runResourceT . runConduit $ toTest
          result `shouldBe` 2
    describe "findPp3" $
      it "should return the file path to the pp3 if it exists" $
        withEmptyTmpDir $ \dir -> do
          [f1Cr2, _, f2Cr2] <- sequenceA $ createFile dir <$> ["file1.CR2", "file1.CR2.pp3", "file2.CR2"]
          result1 <- findPp3 f1Cr2
          result2 <- findPp3 f2Cr2
          isJust result1 `shouldBe` True
          isNothing result2 `shouldBe` True
    describe "isConversionNecessary" $ do
      it "should return true if target pp3 file is not existing" $
        property isConversionNecessaryShouldReturnTrueIfTargetPp3FileIsNotExisting
      it "should return true if target pp3 file is existing but not equal to given pp3" $
        withEmptySourceAndTarget $ \sourceDir targetDir -> do
          sourceFile <- let f = sourceDir </> "test.CR2" in f <$ writeFile f ""
          _ <- writeFile (sourceDir </> "test.pp3") "a"
          _ <- writeFile (targetDir </> "test.pp3") "b"
          defaultPp3 <- let f = sourceDir </> "default.pp3" in Just f <$ writeFile f "b"
          result <- isConversionNecessary sourceFile targetDir defaultPp3
          result `shouldBe` True
      it "should return False if target pp3 file is existing and equal to given pp3" $
        withEmptyTmpDir $ \dir -> do
          (sourceDir, targetDir) <- createSourceAndTarget dir
          sourceFile <- let f = sourceDir </> "test.CR2" in f <$ writeFile f ""
          _ <- writeFile (sourceDir </> "test.CR2.pp3") "a"
          _ <- writeFile (targetDir </> "test.jpg.pp3") "a"
          _ <- writeFile (targetDir </> "test.jpg") ""
          defaultPp3 <- let f = sourceDir </> "default.pp3" in Just f <$ writeFile f "b"
          result <- isConversionNecessary sourceFile targetDir defaultPp3
          result `shouldBe` False

      it "should behave correctly for all cases" $
        property isConversionNecessaryShouldBehaveCorrectlyForAllCases
    describe "parseIni" $
      it "should be able to parse our pp3 file" $ do
        either' <- readIniFile "test/resources/IMG_3695.CR2.pp3"
        ("" :: String) <$ either' `shouldNotBe` Left ""


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
        dirPath rootSourceDir = getTargetDirectoryPath (RootSourceDir (trace ("rootSourceDir = " <> rootSourceDir) rootSourceDir))
                                                       (RootTargetDir (trace ("rootTargetDir = " <> anyString) anyString))
                                                       (trace ("source file path = " <> show fp) fp)
        leftResult = dirPath fpDifferent
        leftResultExpected = Left (SourceFilePathIsNotUnderRootSourceDir (RootSourceDir fpDifferent) fp)
        rightResult = dirPath fpUp
        rightResultExpected = Right anyString :: Either GetTargetDirectoryException String
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


withEmptySourceAndTarget :: (FilePath -> FilePath -> IO x) -> IO x
withEmptySourceAndTarget f =
  withEmptyTmpDir $ createSourceAndTarget >=> uncurry f

createSourceFile :: FilePath -> IO FilePath
createSourceFile sourceDir = let f = sourceDir </> "test.CR2" in f <$ writeFile f ""

withSourceTargetAndSourceFile :: (FilePath -> FilePath -> FilePath -> IO x) -> IO x
withSourceTargetAndSourceFile f =
  withEmptySourceAndTarget $
    \sourceDir targetDir -> createSourceFile sourceDir >>= f sourceDir targetDir

getEmptyTemporaryDirectory :: IO FilePath
getEmptyTemporaryDirectory = do
  randomTempDir <- (</>) <$> getTemporaryDirectory <*> randomString
  existingFileOrDir' <- existingFileOrDir randomTempDir
  emptyDir <- if existingFileOrDir' then getEmptyTemporaryDirectory
              else return randomTempDir
  createDirectory emptyDir
  return emptyDir
  where
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
                                                                  (DefaultPp3ShouldBeGiven defaultPp3ShouldBeGiven) =
  ioProperty . withSourceTargetAndSourceFile  $ \sourceDir targetDir sourceFile -> do
  _ <- when sourceFilePathShouldContainPp3 (writePp3 sourceDir)
  defaultPp3 <- if defaultPp3ShouldBeGiven then let f = sourceDir </> "default.pp3" in Just f <$ writeFile f "" else pure Nothing
  isConversionNecessary sourceFile targetDir defaultPp3

writePp3 :: FilePath -> IO ()
writePp3 sourceDir = writeFile (sourceDir </> "test.pp3") ""

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
isConversionNecessaryShouldBehaveCorrectlyForAllCases (SourceFilePathShouldContainPp3 sourceFilePathShouldContainPp3) -- true
                                                      (TargetFilePathShouldContainPp3 targetFilePathShouldContainPp3) -- true
                                                      (SourcePp3ShouldEqualTargetPp3 sourcePp3ShouldEqualTargetPp3)   -- false
                                                      (TargetFilePathShouldExist targetFilePathShouldExist)           -- true
                                                      (DefaultPp3ShouldBeGiven defaultPp3ShouldBeGiven) =
  ioProperty .withSourceTargetAndSourceFile $ \sourceDir targetDir sourceFile ->
    let targetPp3Content = if sourcePp3ShouldEqualTargetPp3 then "a" else "aa"
    in do
      _ <- when targetFilePathShouldExist $ writeFile (targetDir </> "test.jpg") ""
      _ <- when sourceFilePathShouldContainPp3 $ writeFile (sourceDir </> "test.CR2.pp3") "a"
      _ <- when targetFilePathShouldContainPp3 $ writeFile (targetDir </> "test.jpg.pp3") targetPp3Content
      defaultPp3 <- if defaultPp3ShouldBeGiven then let f = sourceDir </> "default.pp3" in Just f <$ writeFile f "a" else pure Nothing
      result <- isConversionNecessary sourceFile targetDir defaultPp3
      let shouldBeNecessary = not (targetFilePathShouldExist && targetFilePathShouldContainPp3 && (sourcePp3ShouldEqualTargetPp3 || (not sourceFilePathShouldContainPp3 && not defaultPp3ShouldBeGiven)))
      pure $ result == shouldBeNecessary
