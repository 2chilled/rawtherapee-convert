{-# LANGUAGE TupleSections #-}
module Main where

import           Data.Maybe                     ( fromMaybe )
import           System.Log.Logger              ( updateGlobalLogger
                                                , setLevel
                                                , Priority(DEBUG)
                                                , addHandler
                                                , errorM
                                                , infoM
                                                )
import           System.Log.Handler.Syslog      ( openlog
                                                , Option(PID)
                                                , Facility(USER)
                                                )
import           Graphics.RawTherapeeConvert
import           Data.Conduit                   ( (.|)
                                                , runConduit
                                                )
import           Data.Monoid                    ( (<>) )
import           Data.List                      ( intercalate )
import           Control.Monad.Trans.Resource   ( runResourceT )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.Except     ( ExceptT(..)
                                                , runExceptT
                                                , throwE
                                                , mapExceptT
                                                )
import           Control.Monad                  ( void )
import           Control.Applicative            ( (<|>) )
import qualified Data.Conduit.Combinators      as CC
import           Data.Either.Combinators        ( swapEither )
import           System.Console.GetOpt          ( OptDescr(..)
                                                , ArgDescr(..)
                                                , getOpt
                                                , ArgOrder(..)
                                                , usageInfo
                                                )
import           System.Environment             ( getArgs )
import           System.Directory               ( doesDirectoryExist
                                                , doesFileExist
                                                , getPermissions
                                                , executable
                                                , createDirectoryIfMissing
                                                )
import           System.FilePath                ( (</>)
                                                , takeFileName
                                                , replaceExtension
                                                , pathSeparator
                                                )
import           Data.String.Utils              ( strip )
import           Control.Arrow                  ( (&&&) )

main :: IO ()
main = do
  configureLogger
  validateInputs >>= (logInputException `either` convert)

convert :: UserSettings -> IO ()
convert us =
  let sourceDir = usSourceDir us
      cr2Paths' = cr2Paths (LoggerName loggerName) sourceDir
      convertStream =
          cr2Paths' .| CC.mapM_ (lift . convertHelper (RootSourceDir sourceDir))
  in  runResourceT . runConduit $ convertStream
 where
  convertHelper :: RootSourceDir -> SourceFilePath -> IO ()
  convertHelper rootSourceDir sourceFilePath =
    let
      rootTargetDir = RootTargetDir $ usTargetDir us
      targetDirEither =
        getTargetDirectoryPath rootSourceDir rootTargetDir sourceFilePath
      targetDirExceptionLog targetDirException =
        errorM loggerName
          $  "Error, this should have been validated: '"
          <> show targetDirException
          <> "'"
      successfulConversionLog targetDir decision = case decision of
        Converted ->
          infoM loggerName
            $  "Successfully converted "
            <> show sourceFilePath
            <> " to dir: "
            <> show targetDir
        NotConverted ->
          infoM loggerName $ "No need to convert " <> em sourceFilePath
      conversionProcess =
        convertIt sourceFilePath (usDefaultPp3 us) (usDlnaMode us)
    in
      void
      . runExceptT
      . (>>= liftIO . targetDirExceptionLog)
      . swapExceptT
      $ do
          (decision, targetDir) <-
            ExceptT
            $ (\targetDir -> (, targetDir) <$> conversionProcess targetDir)
            `traverse` targetDirEither
          liftIO $ successfulConversionLog targetDir decision

  convertIt
    :: SourceFilePath
    -> Maybe PP3FilePath
    -> DlnaMode
    -> TargetDirPath
    -> IO ConversionDecision
  convertIt sourceFilePath maybePp3FilePath dlnaMode targetDirPath = do
    conversionNecessary <- isConversionNecessary sourceFilePath
                                                 targetDirPath
                                                 maybePp3FilePath
                                                 dlnaMode
    if conversionNecessary
      then do
        createDirectoryIfMissing True targetDirPath
        existingPp3 <- determinePp3FilePath sourceFilePath
        let pp3FilePathToUse = existingPp3 <|> maybePp3FilePath
        Converted
          <$ convertItFinally (usRtExec us)
                              sourceFilePath
                              pp3FilePathToUse
                              dlnaMode
                              targetDirPath
      else pure NotConverted

  convertItFinally
    :: RTExec
    -> SourceFilePath
    -> Maybe PP3FilePath
    -> DlnaMode
    -> TargetDirPath
    -> IO ()
  convertItFinally rtExec sourceFilePath maybePp3FilePath dlnaMode targetDirPath
    = let
        resultErrorLog exception =
          errorM loggerName
            $  "Failed to convert file "
            <> em sourceFilePath
            <> ": "
            <> show exception
        targetFilePath =
          targetDirPath
            </> ((`replaceExtension` "jpg") . takeFileName $ sourceFilePath)
        execRTWithoutPp3' =
          execRTWithoutPp3 rtExec sourceFilePath targetFilePath dlnaMode
        execRT' pp3FilePath =
          execRT rtExec sourceFilePath pp3FilePath targetFilePath dlnaMode
        dryRun = usDryRun us
        execWithPp3 =
          uncurry (<*)
            . (execRT' &&& copyBackResultingPp3
                dlnaMode
                (toPp3FilePath targetFilePath)
              )
      in
        do
          infoM loggerName
            $ (if dryRun then wouldStartConvMsg else startConvMsg)
                sourceFilePath
          resultEither <- if dryRun
            then pure . Right $ ()
            else (execRTWithoutPp3' `maybe` execWithPp3) maybePp3FilePath
          (resultErrorLog `either` (const . pure $ ())) resultEither
   where
    wouldStartConvMsg fp = "Would start conversion of file " <> em fp
    startConvMsg fp = "Starting conversion of file " <> em fp

type InputExceptionEither x = ExceptT InputException IO x

data ConversionDecision = Converted | NotConverted deriving (Show, Eq)

logInputException :: InputException -> IO ()
logInputException e =
  let msg s = if null s then usageInfo' else header <> s <> "\n" <> usageInfo'
  in  case e of
        (InputExceptionSyntax   s) -> putStrLn $ msg s
        (InputExceptionSemantic s) -> putStrLn $ msg s
 where
  header :: String
  header = "Errors:\n"

validateInputs :: IO (Either InputException UserSettings)
validateInputs = getArgs >>= (runExceptT . validateHelper)
 where
  validateHelper :: [String] -> InputExceptionEither UserSettings
  validateHelper args = case getOpt RequireOrder optDescriptions args of
    ([]        , _ , []) -> throwE . InputExceptionSyntax $ ""
    (parsedOpts, [], []) -> do
      parsedUserSettings <- foldToEither parsedOpts >>= liftIO . usWithExecFlag
      ExceptT . pure . validateFlagExistence $ parsedUserSettings
    (_, _, errors) -> throwE . InputExceptionSyntax . unlines $ errors

  foldToEither
    :: [UserSettings -> InputExceptionEither UserSettings]
    -> InputExceptionEither UserSettings
  foldToEither = foldl (>>=) (pure emptyUserSettings)

  validateFlagExistence :: UserSettings -> Either InputException UserSettings
  validateFlagExistence us =
    let mustBeProvidedMsg option = option <> " option must be provided"
        errorTuples =
            [ (usSourceDir, mustBeProvidedMsg "-b")
            , (usTargetDir, mustBeProvidedMsg "-t")
            , (usRtExec   , mustBeProvidedMsg "-e")
            ]
        exceptionStrings = do
          (f, errorString) <- errorTuples
          [ errorString | f us == "" ]
        exception = InputExceptionSyntax $ intercalate "\n" exceptionStrings
    in  if null exceptionStrings then Right us else Left exception

  usWithExecFlag :: UserSettings -> IO UserSettings
  usWithExecFlag us = if usRtExec us == ""
    then
      let result =
            (fmap . fmap) (\path -> us { usRtExec = path }) probeRtInSysPath
      in  fromMaybe us <$> result
    else pure us

optDescriptions
  :: [OptDescr (UserSettings -> InputExceptionEither UserSettings)]
optDescriptions =
  [ Option
    ['b']
    ["baseDir"]
    (ReqArg
      (\sourceDir us ->
        (\fp -> us { usSourceDir = fp }) <$> directoryValidation sourceDir
      )
      "/home/user/pics"
    )
    "Base dir to look for raw files"
  , Option
    ['t']
    ["targetDir"]
    (ReqArg
      (\targetDir us ->
        (\fp -> us { usTargetDir = fp }) <$> directoryValidation targetDir
      )
      "/home/user/pics_converted"
    )
    "Target dir where converted pictures will be saved to"
  , Option
    ['e']
    ["executable"]
    (ReqArg
      (\executablePath us ->
        (\e -> us { usRtExec = e }) <$> executableValidation executablePath
      )
      "/usr/bin/rawtherapee"
    )
    "Path to the rawtherapee executable"
  , Option
    ['n']
    ["dryRun"]
    (NoArg (\us -> pure $ us { usDryRun = True }))
    "(Optional) Enable dry run doing nothing but printing what would be done"
  , Option
    ['d']
    ["dlnaMode"]
    (NoArg (\us -> pure $ us { usDlnaMode = True }))
    "(Optional) Enable DLNA mode to limit resolution of converted pictures to match DLNA constraints"
  ]
 where
  directoryValidation :: FilePath -> InputExceptionEither FilePath
  directoryValidation fp = do
    doesExist <- liftIO $ doesDirectoryExist fp
    if doesExist
      then pure (appendSlash . strip $ fp)
      else throwE . InputExceptionSemantic $ em fp <> " is not a directory"

  executableValidation :: FilePath -> InputExceptionEither FilePath
  executableValidation fp = do
    doesExist    <- liftIO $ doesFileExist fp
    isExecutable <- liftIO
      $ if doesExist then executable <$> getPermissions fp else pure False
    case (doesExist, isExecutable) of
      (False, _) -> throwE . InputExceptionSemantic $ em fp <> " is not a file"
      (_, False) ->
        throwE . InputExceptionSemantic $ em fp <> " is not executable"
      _ -> pure (strip fp)

  appendSlash :: String -> String
  appendSlash =
    reverse . (pathSeparator :) . dropWhile (== pathSeparator) . reverse

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
  -- |DLNA mode to automatically resize images to maximum resolution supported by dlna protocol
, usDlnaMode :: Bool
  -- |Full path to the rawtherapee executable
, usRtExec :: RTExec
  -- |Whether we should really do something
, usDryRun :: Bool
} deriving (Show, Eq)

emptyUserSettings :: UserSettings
emptyUserSettings = UserSettings { usSourceDir  = ""
                                 , usTargetDir  = ""
                                 , usDefaultPp3 = Nothing
                                 , usRtExec     = ""
                                 , usDryRun     = False
                                 , usDlnaMode   = False
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

swapExceptT :: Functor m => ExceptT e m a -> ExceptT a m e
swapExceptT = mapExceptT (fmap swapEither)
