module Graphics.RawTherapeeConvert where

import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import qualified Data.Conduit.Combinators as CC
import Data.Conduit (($$), Source)
import qualified Data.Text as T
import Data.Text (unpack, pack, Text, breakOn)

newtype RootSourceDir = RootSourceDir FilePath

newtype RootTargetDir = RootTargetDir FilePath

type SourceFilePath = FilePath

type TargetDirPath = FilePath

filePaths :: MonadResource m => FilePath -> Source m FilePath
filePaths = CC.sourceDirectoryDeep False 

tmpPaths :: MonadResource m => Source m FilePath
tmpPaths = filePaths "/home/chief/Medien/Bilder"

printTmpPaths :: IO ()
printTmpPaths = runResourceT $ tmpPaths $$ CC.print 

getTargetDirectoryPath :: RootSourceDir -> RootTargetDir -> SourceFilePath -> TargetDirPath
getTargetDirectoryPath (RootSourceDir rootSourceDir) 
                       (RootTargetDir rootTargetDir) 
                       sourceFilePath = unpack $ replaceOne (pack rootSourceDir) 
                                                            (pack rootTargetDir) 
                                                            (pack sourceFilePath)
  where replaceOne :: Text -> Text -> Text -> Text
        replaceOne pattern substitution text
          | T.null back = text    -- pattern doesn't occur
          | otherwise = T.concat [front, substitution, T.drop (T.length pattern) back] 
            where (front, back) = breakOn pattern text

