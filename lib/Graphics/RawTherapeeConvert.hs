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
import System.Log.Logger (updateGlobalLogger, setLevel, Priority (DEBUG), addHandler, infoM)
import System.Log.Handler.Syslog (openlog, Option (PID), Facility (USER))
import System.FilePath (takeExtension, (<.>))
import System.Directory (doesFileExist)

newtype RootSourceDir = RootSourceDir FilePath

newtype RootTargetDir = RootTargetDir FilePath

type SourceFilePath = FilePath

type TargetDirPath = FilePath

filePaths :: (MonadResource m, MonadBaseControl IO m) => FilePath -> Source m FilePath
filePaths = errorHandled . (CC.sourceDirectoryDeep False)
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

cr2Paths :: (MonadResource m, MonadBaseControl IO m) => FilePath -> Source m FilePath
cr2Paths fp = filePaths fp =$= CC.filter ((== ".CR2") . takeExtension)

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
              in do
                doesExist <- doesFileExist pp3
                return $ if doesExist then Just pp3 else Nothing

--Logging

loggerName :: String
loggerName = "Graphics.RawTherapeeConvert"

programName :: String
programName = "rawtherapee-convert"

configureLogger :: IO ()
configureLogger = do
  sysLogHandler <- openlog programName [PID] USER DEBUG
  updateGlobalLogger loggerName (setLevel DEBUG . addHandler sysLogHandler)

