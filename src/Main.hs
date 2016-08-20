module Main where

import System.Log.Logger (updateGlobalLogger, setLevel, Priority (DEBUG), addHandler, errorM, infoM)
import System.Log.Handler.Syslog (openlog, Option (PID), Facility (USER))
import Graphics.RawTherapeeConvert
import Data.Conduit (Source, (=$=), runConduit)
import Data.Monoid ((<>))
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT(..), runEitherT, left)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<|>))
import qualified Data.Conduit.Combinators as CC
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), getOpt, ArgOrder(..), usageInfo)
import System.Environment (getArgs)
import System.Directory (doesDirectoryExist)
import Data.String.Utils (strip)

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

type InputExceptionEither x = EitherT InputException IO x

logInputException :: InputException -> IO ()
logInputException e = let msg s = header <> s <> "\n" <> usageInfo'
                      in case e of (InputExceptionSyntax s)   -> putStrLn $ msg s
                                   (InputExceptionSemantic s) -> putStrLn $ msg s
  where header :: String
        header = "Errors:\n"

validateInputs :: IO (Either InputException UserSettings)
validateInputs = getArgs >>= (runEitherT . validateHelper)
  where validateHelper :: [String] -> InputExceptionEither UserSettings
        validateHelper args = case getOpt RequireOrder optDescriptions args of
          ([], _, []) -> left . InputExceptionSyntax $ ""
          (parsedOpts, [], []) -> foldToEither parsedOpts
          (_, _, errors) -> left . InputExceptionSyntax . unlines $ errors

        foldToEither :: [UserSettings -> InputExceptionEither UserSettings] -> InputExceptionEither UserSettings
        foldToEither = foldl (>>=) (pure emptyUserSettings)

optDescriptions :: [OptDescr (UserSettings -> InputExceptionEither UserSettings)]
optDescriptions = [
  Option ['b'] ["baseDir"]
  (ReqArg (\sourceDir us -> (\fp -> us { usSourceDir = fp }) <$> sourceDirValidation sourceDir ) "/home/user/pics")
  "Base dir to look for raw files"
  ]
  where sourceDirValidation :: FilePath -> InputExceptionEither FilePath
        sourceDirValidation fp = do
          doesExist <- liftIO $ doesDirectoryExist fp
          if doesExist then pure (strip fp) else left . InputExceptionSemantic $ em fp <> " is not a directory"

usageInfo' :: String
usageInfo' = usageInfo "Usage: rawtherapee-convert [OPTION...]" optDescriptions

data InputException = InputExceptionSyntax String
                    | InputExceptionSemantic String

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

emptyUserSettings :: UserSettings
emptyUserSettings = UserSettings {
  usSourceDir = ""
, usTargetDir = ""
, usDefaultPp3 = Nothing
, usRtExec = ""
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
