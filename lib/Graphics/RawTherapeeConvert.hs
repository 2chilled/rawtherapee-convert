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
  determinePp3FilePath
) where

import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)
import Control.Monad.Base (liftBase)
import Data.Monoid ((<>))
import qualified Data.Conduit.Combinators as CC
import Data.Conduit ((=$=), Source, handleC, yield)
import qualified Data.Text as T
import Data.Text (unpack, pack, Text, breakOn)
import Data.Maybe (fromMaybe)
import Control.Exception (IOException, try)
import Data.String.Utils (startswith)
import System.Log.Logger (infoM)
import System.FilePath (takeExtension, (<.>))
import System.Directory (doesFileExist)
import System.Process (callProcess)

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

data GetTargetDirectoryException = SourceFilePathIsNotUnderRootSourceDir
  deriving (Show, Eq)

getTargetDirectoryPath :: RootSourceDir
                       -> RootTargetDir
                       -> SourceFilePath
                       -> Either GetTargetDirectoryException TargetDirPath
getTargetDirectoryPath (RootSourceDir rootSourceDir)
                       (RootTargetDir rootTargetDir)
                       sourceFilePath | sourceFilePath `startswith` rootSourceDir =
                                                    Right . unpack $ replaceOne (pack rootSourceDir)
                                                                                (pack rootTargetDir)
                                                                                (pack sourceFilePath)
                                      | otherwise = Left SourceFilePathIsNotUnderRootSourceDir

  where replaceOne :: Text -> Text -> Text -> Text
        replaceOne pattern substitution text
          | T.null back = text    -- pattern doesn't occur
          | otherwise = T.concat [front, substitution, T.drop (T.length pattern) back]
            where (front, back) = breakOn pattern text

targetFilePathAlreadyExists :: SourceFilePath -> TargetFilePath -> IO Bool
targetFilePathAlreadyExists = undefined

isConversionNecessary :: SourceFilePath -> TargetFilePath -> Maybe PP3FilePath -> IO Bool
isConversionNecessary = undefined

determinePp3FilePath :: SourceFilePath -> IO (Maybe PP3FilePath)
determinePp3FilePath = undefined

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
