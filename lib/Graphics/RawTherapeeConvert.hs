{-# LANGUAGE FlexibleContexts #-}
module Graphics.RawTherapeeConvert where

import Control.Monad.Trans.Resource (MonadResource, runResourceT, MonadBaseControl, ResourceT)
import Control.Monad.Base (liftBase)
import Data.Monoid ((<>))
import qualified Data.Conduit.Combinators as CC
import Data.Conduit ((=$=), ($$), Source, handleC, yield)
import qualified Data.Text as T
import Data.Text (unpack, pack, Text, breakOn)
import Control.Exception (IOException)
import Data.String.Utils (startswith)

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
                               printMsg = liftBase . putStrLn . msg
                               doNothing = const . liftBase . return $ ()
                           in CC.iterM $ printMsg `either` doNothing
              catched = handleC (\e -> yield (Left (e :: IOException)))
          in catched (conduit =$= eitherConduit) =$= logConduit =$= maybeConduit =$= filteredBySome

tmpPaths :: Source (ResourceT IO) FilePath
tmpPaths = filePaths "/tmp"

printTmpPaths :: IO ()
printTmpPaths = runResourceT $ tmpPaths $$ CC.print

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

