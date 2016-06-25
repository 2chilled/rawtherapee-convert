import Test.Framework
import qualified Graphics.RawTherapeeConvert.Test as T
 
main :: IO ()
main = defaultMain tests
 
tests :: [Test]
tests =
  [
   testGroup "Main test group"
     T.tests
  ]
