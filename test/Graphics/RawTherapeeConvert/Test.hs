module Graphics.RawTherapeeConvert.Test where
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, Arbitrary, arbitrary, property)
import Data.Monoid ((<>))
import System.FilePath (pathSeparator, takeDirectory)
import Graphics.RawTherapeeConvert

tests :: [Test]
tests = [
  testGroup "Graphics.RawTherapeeConvert" [
    testProperty "getTargetDirectoryPath should return a left if sourceFilePath is not a subpath of root source dir"
      getTargetDirectoryPathShouldReturnALeftIfSourceFilePathIsNotASubpathOfRootSourceDir
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
                                in property $ result == Left(SourceFilePathIsNotUnderRootSourceDir)
