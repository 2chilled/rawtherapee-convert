module Main where

import System.Log.Logger (updateGlobalLogger, setLevel, Priority (DEBUG), addHandler, errorM, infoM)
import System.Log.Handler.Syslog (openlog, Option (PID), Facility (USER))
import Graphics.RawTherapeeConvert
import Data.Conduit (Source, (=$=), runConduit)
import Data.Monoid ((<>))
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Trans.Class (lift)
import Control.Applicative ((<|>))
import qualified Data.Conduit.Combinators as CC

main :: IO ()
main = validateInputs >>= (logInputException `either` convert)

convert :: UserSettings -> IO ()
convert us =
  let sourceDir = usSourceDir us
      cr2Paths' = cr2Paths (LoggerName loggerName) sourceDir :: Source (ResourceT IO) FilePath
      convertStream = cr2Paths' =$= CC.mapM_ (lift . convertHelper (RootSourceDir sourceDir))
  in runResourceT . runConduit $ convertStream
  where convertHelper :: RootSourceDir -> SourceFilePath -> IO ()
        convertHelper rootSourceDir sourceFilePath =
          let rootTargetDir = RootTargetDir $ usTargetDir us
              targetDirEither = getTargetDirectoryPath rootSourceDir rootTargetDir sourceFilePath
              targetDirExceptionLog targetDirException = errorM loggerName $
                "Error, this should have been validated: '" <> show targetDirException <> "'"
              successfulConversionLog targetDir = infoM loggerName $
                "Successfully converted from " <> show rootSourceDir <> " to " <> show targetDir
              conversionProcess = convertIt sourceFilePath (usDefaultPp3 us) `traverse` targetDirEither
          in do
            conversionResult <- (*> targetDirEither) <$> conversionProcess
            (targetDirExceptionLog `either` successfulConversionLog) conversionResult

        convertIt :: SourceFilePath -> Maybe PP3FilePath -> TargetDirPath -> IO ()
        convertIt sourceFilePath maybePp3FilePath targetDirPath = do
          conversionNecessary <- isConversionNecessary sourceFilePath targetDirPath maybePp3FilePath
          if conversionNecessary then do
            existingPp3 <- determinePp3FilePath sourceFilePath
            let pp3FilePathToUse = existingPp3 <|> maybePp3FilePath
            convertItFinally (usRtExec us) sourceFilePath pp3FilePathToUse targetDirPath
          else
            infoM loggerName $ "No need to convert '" <> sourceFilePath <> "'"

        convertItFinally :: RTExec -> SourceFilePath -> Maybe PP3FilePath -> TargetDirPath -> IO ()
        convertItFinally rtExec sourceFilePath maybePp3FilePath targetDirPath =
          let resultErrorLog exception = errorM loggerName $ "Failed to convert file '" <> sourceFilePath <> "': " <> show exception
              resultSuccessLog = infoM loggerName $ "Successfully converted file '" <> sourceFilePath <> "' to '" <> targetDirPath <> "'"
              execRTWithoutPp3' = execRTWithoutPp3 rtExec sourceFilePath targetDirPath
              execRT' pp3FilePath = execRT rtExec sourceFilePath pp3FilePath targetDirPath
          in do
            infoM loggerName $ "Starting conversion of file '" <> sourceFilePath <> "'"
            resultEither <- (execRTWithoutPp3' `maybe` execRT') maybePp3FilePath
            (resultErrorLog `either` const resultSuccessLog) resultEither

logInputException :: InputException -> IO ()
logInputException = undefined

validateInputs :: IO (Either InputException UserSettings)
validateInputs = undefined

data InputException

data UserSettings = UserSettings {
  -- |Base dir we'll look for cr2 files
  usSourceDir :: FilePath
  -- |Target dir under which we'll mirror the 'sourceDir' structure to place the converted pictures
, usTargetDir :: FilePath
  -- |Default pp3 file we'll use for converting when there's no file specific one found
, usDefaultPp3 :: Maybe PP3FilePath
  -- |Full path to the rawtherapee executable
, usRtExec :: RTExec
}

--Logging

loggerName :: String
loggerName = "Graphics.RawTherapeeConvert"

programName :: String
programName = "rawtherapee-convert"

configureLogger :: IO ()
configureLogger = do
  sysLogHandler <- openlog programName [PID] USER DEBUG
  updateGlobalLogger loggerName (setLevel DEBUG . addHandler sysLogHandler)
