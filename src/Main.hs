{-# LANGUAGE TupleSections #-}
module Main where

import System.Log.Logger (updateGlobalLogger, setLevel, Priority (DEBUG), addHandler, errorM, infoM)
import System.Log.Handler.Syslog (openlog, Option (PID), Facility (USER))
import Graphics.RawTherapeeConvert
import Data.Conduit (Source, (=$=), runConduit)
import Data.Monoid ((<>))
import Data.List (intercalate)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT(..), runEitherT, left, swapEitherT, hoistEither)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Control.Applicative ((<|>))
import qualified Data.Conduit.Combinators as CC
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), getOpt, ArgOrder(..), usageInfo)
import System.Environment (getArgs)
import System.Directory (doesDirectoryExist, doesFileExist, getPermissions, executable, createDirectoryIfMissing, copyFile)
import System.FilePath ((</>), takeFileName, replaceExtension, pathSeparator)
import Data.String.Utils (strip)
import Control.Arrow ((&&&))

main :: IO ()
main = do
  configureLogger
  validateInputs >>= (logInputException `either` convert)

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
              successfulConversionLog targetDir decision =
                case decision
                  of Converted    -> infoM loggerName $
                       "Successfully converted from " <> show rootSourceDir <> " to " <> show targetDir
                     NotConverted -> infoM loggerName $
                       "No need to convert " <> em sourceFilePath
              conversionProcess = convertIt sourceFilePath (usDefaultPp3 us)
          in void . runEitherT . (>>= liftIO . targetDirExceptionLog) . swapEitherT $ do
            (decision, targetDir) <- EitherT $ (\targetDir -> (,targetDir) <$> conversionProcess targetDir) `traverse` targetDirEither
            liftIO $ successfulConversionLog targetDir decision

        convertIt :: SourceFilePath -> Maybe PP3FilePath -> TargetDirPath -> IO ConversionDecision
        convertIt sourceFilePath maybePp3FilePath targetDirPath = do
          conversionNecessary <- isConversionNecessary sourceFilePath targetDirPath maybePp3FilePath
          if conversionNecessary then do
            createDirectoryIfMissing True targetDirPath
            existingPp3 <- determinePp3FilePath sourceFilePath
            let pp3FilePathToUse = existingPp3 <|> maybePp3FilePath
            Converted <$ convertItFinally (usRtExec us) sourceFilePath pp3FilePathToUse targetDirPath
          else
            pure NotConverted

        convertItFinally :: RTExec -> SourceFilePath -> Maybe PP3FilePath -> TargetDirPath -> IO ()
        convertItFinally rtExec sourceFilePath maybePp3FilePath targetDirPath =
          let resultErrorLog exception = errorM loggerName $ "Failed to convert file '" <> sourceFilePath <> "': " <> show exception
              resultSuccessLog = infoM loggerName $ "Successfully converted file '" <> sourceFilePath <> "' to '" <> targetFilePath <> "'"
              targetFilePath = targetDirPath </> ((`replaceExtension` "jpg") . takeFileName $ sourceFilePath)
              copyBackResultingPp3 = copyFile (toPp3FilePath targetFilePath)
              execRTWithoutPp3' = execRTWithoutPp3 rtExec sourceFilePath targetFilePath
              execRT' pp3FilePath = execRT rtExec sourceFilePath pp3FilePath targetFilePath
          in do
            infoM loggerName $ "Starting conversion of file '" <> sourceFilePath <> "'"
            resultEither <- (execRTWithoutPp3' `maybe` (uncurry (<*) . (execRT' &&& copyBackResultingPp3))) maybePp3FilePath
            (resultErrorLog `either` const resultSuccessLog) resultEither

type InputExceptionEither x = EitherT InputException IO x

data ConversionDecision = Converted | NotConverted deriving (Show, Eq)

logInputException :: InputException -> IO ()
logInputException e = let msg s = if null s then usageInfo' else header <> s <> "\n" <> usageInfo'
                      in case e of (InputExceptionSyntax s)   -> putStrLn $ msg s
                                   (InputExceptionSemantic s) -> putStrLn $ msg s
  where header :: String
        header = "Errors:\n"

validateInputs :: IO (Either InputException UserSettings)
validateInputs = getArgs >>= (runEitherT . validateHelper)
  where validateHelper :: [String] -> InputExceptionEither UserSettings
        validateHelper args = case getOpt RequireOrder optDescriptions args of
          ([], _, []) -> left . InputExceptionSyntax $ ""
          (parsedOpts, [], []) -> do
            parsedUserSettings <-foldToEither parsedOpts >>= liftIO . usWithExecFlag
            hoistEither . validateFlagExistence $ parsedUserSettings
          (_, _, errors) -> left . InputExceptionSyntax . unlines $ errors

        foldToEither :: [UserSettings -> InputExceptionEither UserSettings] -> InputExceptionEither UserSettings
        foldToEither = foldl (>>=) (pure emptyUserSettings)

        validateFlagExistence :: UserSettings -> Either InputException UserSettings
        validateFlagExistence us =
          let mustBeProvidedMsg option = option <> " option must be provided"
              errorTuples = [ (usSourceDir, mustBeProvidedMsg "-b")
                            , (usTargetDir, mustBeProvidedMsg "-t")
                            , (usRtExec, mustBeProvidedMsg "-e")
                            ]
              exceptionStrings = do
                (f, errorString) <- errorTuples
                if f us == "" then [errorString] else []
              exception = InputExceptionSyntax $ intercalate "\n" exceptionStrings
          in if null exceptionStrings then Right us else Left exception

        usWithExecFlag :: UserSettings -> IO UserSettings
        usWithExecFlag us =
          if usRtExec us == ""
          then let result = (fmap . fmap) (\path -> us {usRtExec = path}) probeRtInSysPath
               in maybe us id <$> result
          else pure us

optDescriptions :: [OptDescr (UserSettings -> InputExceptionEither UserSettings)]
optDescriptions = [
    Option ['b'] ["baseDir"]
    (ReqArg (\sourceDir us -> (\fp -> us { usSourceDir = fp }) <$> directoryValidation sourceDir ) "/home/user/pics")
    "Base dir to look for raw files"

  , Option ['t'] ["targetDir"]
    (ReqArg (\targetDir us -> (\fp -> us { usTargetDir = fp }) <$> directoryValidation targetDir ) "/home/user/pics_converted")
    "Target dir where converted pictures will be saved to"

  , Option ['e'] ["executable"]
    (ReqArg (\executablePath us -> (\e -> us { usRtExec = e }) <$> executableValidation executablePath ) "/usr/bin/rawtherapee")
    "Target dir where converted pictures will be saved to"

  , Option ['n'] ["dryRun"]
    (NoArg (\us -> pure $ us { usDryRun = True }))
    "(Optional) Enable dry run doing nothing but printing what would be done"
  ]
  where directoryValidation :: FilePath -> InputExceptionEither FilePath
        directoryValidation fp = do
          doesExist <- liftIO $ doesDirectoryExist fp
          if doesExist then pure (appendSlash . strip $ fp) else left . InputExceptionSemantic $ em fp <> " is not a directory"

        executableValidation :: FilePath -> InputExceptionEither FilePath
        executableValidation fp = do
          doesExist <- liftIO $ doesFileExist fp
          isExecutable <- liftIO $ if doesExist then executable <$> getPermissions fp else pure False
          case (doesExist, isExecutable)
            of (False, _) -> left . InputExceptionSemantic $ em fp <> " is not a file"
               (_, False) -> left . InputExceptionSemantic $ em fp <> " is not executable"
               _          -> pure (strip fp)

        appendSlash :: String -> String
        appendSlash = reverse . (pathSeparator:) . dropWhile (== pathSeparator) . reverse

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
, usDryRun :: Bool
} deriving (Show, Eq)

emptyUserSettings :: UserSettings
emptyUserSettings = UserSettings {
  usSourceDir = ""
, usTargetDir = ""
, usDefaultPp3 = Nothing
, usRtExec = ""
, usDryRun = False
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
