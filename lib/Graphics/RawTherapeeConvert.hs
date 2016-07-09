{-# LANGUAGE FlexibleContexts #-}
module Graphics.RawTherapeeConvert where

import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)
import Control.Monad.Base (liftBase)
import Data.Monoid ((<>))
import qualified Data.Conduit.Combinators as CC
import Data.Conduit ((=$=), Source, handleC, yield)
import qualified Data.Text as T
import Data.Text (unpack, pack, Text, breakOn)
import Control.Exception (IOException)
import Data.String.Utils (startswith)
import System.Log.Logger (infoM)
import System.FilePath (takeExtension, (<.>))
import System.Directory (doesFileExist)

newtype RootSourceDir = RootSourceDir FilePath

newtype RootTargetDir = RootTargetDir FilePath

type SourceFilePath = FilePath

type TargetDirPath = FilePath

newtype LoggerName = LoggerName String

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

type CR2FilePath = FilePath
type PP3FilePath = FilePath

findPp3 :: CR2FilePath -> IO (Maybe PP3FilePath)
findPp3 cr2 = let pp3 = cr2 <.> "pp3"
                  toMaybe p = if p then Just pp3 else Nothing
              in toMaybe <$> doesFileExist pp3

