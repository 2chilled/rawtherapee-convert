{-# LANGUAGE FlexibleContexts #-}
module Graphics.RawTherapeeConvert (
  filePaths,
  RootSourceDir(..),
  RootTargetDir(..),
  GetTargetDirectoryException(..),
  SourceFilePath,
  TargetDirPath,
  LoggerName(..),
  cr2Paths,
  getTargetDirectoryPath,
  CR2FilePath,
  PP3FilePath,
  findPp3,
  RTExec,
  TargetFilePath,
  execRT,
  execRTWithoutPp3,
  isConversionNecessary,
  determinePp3FilePath,
  em
) where

import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)
import Control.Monad.Trans.Either (EitherT(..), runEitherT)
import Control.Monad.Base (liftBase)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Control.Applicative ((<|>))
import Data.Monoid ((<>), All(..), getAll)
import qualified Data.Conduit.Combinators as CC
import Data.Conduit ((=$=), Source, handleC, yield)
import qualified Data.Text as T
import Data.Text (unpack, pack, Text, breakOn)
import Data.Maybe (fromMaybe)
import Control.Exception (IOException, try)
import Data.String.Utils (startswith)
import System.Log.Logger (infoM)
import System.FilePath (takeExtension, (<.>), (</>), takeFileName, dropExtension)
import System.Directory (doesFileExist)
import System.Process (callProcess)
import qualified Data.ByteString.Lazy as B

newtype RootSourceDir = RootSourceDir FilePath deriving (Show, Eq)

newtype RootTargetDir = RootTargetDir FilePath deriving (Show, Eq)

type SourceFilePath = FilePath

type TargetDirPath = FilePath

newtype LoggerName = LoggerName String deriving (Show, Eq)

filePaths :: (MonadResource m, MonadBaseControl IO m) => LoggerName -> FilePath -> Source m FilePath
filePaths (LoggerName loggerName) = errorHandled . (CC.sourceDirectoryDeep False)
  where errorHandled conduit =
          let eitherConduit = CC.map Right
              maybeConduit = CC.map (const Nothing `either` Just)
              filteredBySome = CC.concatMap id
              logConduit = let msg exception = "Could not access file: " <> show exception
                               printMsg = liftBase . infoM loggerName . msg
                               doNothing = const . return $ ()
                           in CC.iterM $ printMsg `either` doNothing
              catched = handleC (\e -> yield (Left (e :: IOException)))
          in catched (conduit =$= eitherConduit) =$= logConduit =$= maybeConduit =$= filteredBySome

cr2Paths :: (MonadResource m, MonadBaseControl IO m) => LoggerName -> FilePath -> Source m FilePath
cr2Paths ln fp = filePaths ln fp =$= CC.filter ((== ".CR2") . takeExtension)

data GetTargetDirectoryException = SourceFilePathIsNotUnderRootSourceDir RootSourceDir SourceFilePath
  deriving (Show, Eq)

getTargetDirectoryPath :: RootSourceDir
                       -> RootTargetDir
                       -> SourceFilePath
                       -> Either GetTargetDirectoryException TargetDirPath
getTargetDirectoryPath (RootSourceDir rootSourceDir)
                       (RootTargetDir rootTargetDir)
                       sourceFilePath | rootSourceDir `startswith` sourceFilePath =
                                                    Right . unpack $ replaceOne (pack rootSourceDir)
                                                                                (pack rootTargetDir)
                                                                                (pack sourceFilePath)
                                      | otherwise = Left $ SourceFilePathIsNotUnderRootSourceDir (RootSourceDir rootSourceDir) sourceFilePath

  where replaceOne :: Text -> Text -> Text -> Text
        replaceOne pattern substitution text
          | T.null back = text    -- pattern doesn't occur
          | otherwise = T.concat [front, substitution, T.drop (T.length pattern) back]
            where (front, back) = breakOn pattern text

isConversionNecessary :: SourceFilePath -> TargetDirPath -> Maybe PP3FilePath -> IO Bool
isConversionNecessary sourceFilePath targetDirPath maybeDefaultPp3FilePath =
  let targetFilePathExists = let targetFilePath = buildTargetFilePath
                             in  assertFileExists targetFilePath (logTargetFilePathDoesNotExistMsg targetFilePath)
      targetPp3FilePathExists = let targetPp3FilePath = buildTargetPp3FilePath
                                in  assertFileExists targetPp3FilePath (logPp3TargetFilePathDoesNotExistMsg targetPp3FilePath)
      targetPp3FileEqualsGivenPp3File maybeSourcePp3FilePath =
        getAll . foldMap All <$> equalsTargetPp3FilePath `traverse` (maybeSourcePp3FilePath <|> maybeDefaultPp3FilePath)
      result = do
        _ <- targetFilePathExists
        _ <- targetPp3FilePathExists
        targetPp3FileEqualsGivenPp3File' <- liftIO $ sourcePp3FilePath >>= targetPp3FileEqualsGivenPp3File
        pure $ not targetPp3FileEqualsGivenPp3File'
  in (id `either` id) <$> runEitherT result

  where buildTargetFilePath :: TargetFilePath
        buildTargetFilePath = targetDirPath </> dropExtension (takeFileName sourceFilePath) <.> "jpg"

        buildTargetPp3FilePath :: TargetFilePath
        buildTargetPp3FilePath = toPp3FilePath buildTargetFilePath

        sourcePp3FilePath :: IO (Maybe PP3FilePath)
        sourcePp3FilePath = let p = toPp3FilePath sourceFilePath
                            in (\doesExist -> if doesExist then Just p else Nothing) <$> doesFileExist p

        assertFileExists :: FilePath -> IO x -> EitherT Bool IO Bool
        assertFileExists fp ifNot = let handler fileExists' = if fileExists'
                                                              then Right True
                                                              else Left $ True <$ ifNot
                              in  EitherT . joinLeftSide $ handler <$> doesFileExist fp
          where joinLeftSide :: IO (Either (IO a) b) -> IO (Either a b)
                joinLeftSide io = io >>= ((Left <$>) `either` (pure . Right))


        equalsTargetPp3FilePath :: PP3FilePath -> IO Bool
        equalsTargetPp3FilePath sourcePp3 =
          let targetPp3 = buildTargetPp3FilePath
              result = do
                targetPp3Exists <- liftIO $ doesFileExist targetPp3
                _ <- if targetPp3Exists
                     then EitherT . pure $ Right ()
                     else EitherT $ Left () <$ logTargetPp3FilePathDoesNotExistMsg targetPp3
                [sourcePp3', targetPp3'] <- liftIO $ B.readFile `traverse` [sourcePp3, targetPp3]
                let result' = sourcePp3' == targetPp3'
                _ <- when (not result') . liftIO . putStrLn $ ("source pp3 file " <> em sourcePp3 <> " does not equal " <> em targetPp3)
                pure result'
          in (const False `either` id) <$> runEitherT result

        logTargetFilePathDoesNotExistMsg :: TargetFilePath -> IO ()
        logTargetFilePathDoesNotExistMsg targetFilePath =
          putStrLn $ "Target file path " <> em targetFilePath <> " does not exist for source file " <> em sourceFilePath

        logPp3TargetFilePathDoesNotExistMsg :: TargetFilePath -> IO ()
        logPp3TargetFilePathDoesNotExistMsg fp =
          putStrLn $ "Target pp3 file path " <> em fp <> " does not exist for source file " <> em sourceFilePath

        logTargetPp3FilePathDoesNotExistMsg :: PP3FilePath -> IO ()
        logTargetPp3FilePathDoesNotExistMsg targetPp3FilePath =
          putStrLn $ "Target pp3 file path " <> em targetPp3FilePath <> " does not exist for source file " <> em sourceFilePath

em :: String -> String
em s = "'" <> s <> "'"

determinePp3FilePath :: SourceFilePath -> IO (Maybe PP3FilePath)
determinePp3FilePath sourceFilePath =
  let pp3FilePath = toPp3FilePath sourceFilePath
  in do
    doesFileExist' <- doesFileExist pp3FilePath
    pure $ if doesFileExist' then Just pp3FilePath else Nothing

toPp3FilePath :: FilePath -> PP3FilePath
toPp3FilePath fp = dropExtension fp <.> "pp3"

type CR2FilePath = FilePath
type PP3FilePath = FilePath

findPp3 :: CR2FilePath -> IO (Maybe PP3FilePath)
findPp3 cr2 = let pp3 = cr2 <.> "pp3"
                  toMaybe p = if p then Just pp3 else Nothing
              in toMaybe <$> doesFileExist pp3

type RTExec = FilePath
type TargetFilePath = FilePath

execRT :: RTExec
       -> CR2FilePath
       -> PP3FilePath
       -> TargetFilePath
       -> IO (Either IOException ())
execRT executable cr2Path pp3Path targetFilePath =
  let params = toRtCliOptionList $ RtCliOptions {
      rcoCr2FilePath = cr2Path
    , rcoTargetFilePath = targetFilePath
    , rcoPp3FilePath = Just pp3Path
  }
  in callProcess' executable params

execRTWithoutPp3 :: RTExec
                 -> CR2FilePath
                 -> TargetFilePath
                 -> IO (Either IOException ())
execRTWithoutPp3 executable cr2Path targetFilePath =
  let params = toRtCliOptionList $ RtCliOptions {
      rcoCr2FilePath = cr2Path
    , rcoTargetFilePath = targetFilePath
    , rcoPp3FilePath = Nothing
  }
  in callProcess' executable params

-- private

data RtCliOptions = RtCliOptions {
  rcoCr2FilePath :: String
, rcoTargetFilePath :: String
, rcoPp3FilePath :: Maybe String
}

toRtCliOptionList :: RtCliOptions -> [String]
toRtCliOptionList opts = [
      "-O", rcoTargetFilePath opts
    , "-Y"
  ]
  <> ["-d"] `fromMaybe` (("-p":) . pure <$> rcoPp3FilePath opts)
  <> ["-c", rcoCr2FilePath opts]

callProcess' :: String -> [String] -> IO (Either IOException ())
callProcess' executable args = try $ callProcess executable args

  --rawtherapee -O converted/ -s -d -p /home/chief/.config/RawTherapee/profiles/my_default.pp3 -c IMG_0105.CR2
