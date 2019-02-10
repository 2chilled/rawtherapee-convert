{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Graphics.RawTherapeeConvert
  ( filePaths
  , RootSourceDir(..)
  , RootTargetDir(..)
  , GetTargetDirectoryException(..)
  , SourceFilePath
  , TargetDirPath
  , LoggerName(..)
  , cr2Paths
  , getTargetDirectoryPath
  , CR2FilePath
  , PP3FilePath
  , findPp3
  , RTExec
  , TargetFilePath
  , execRT
  , execRTWithoutPp3
  , isConversionNecessary
  , determinePp3FilePath
  , em
  , toPp3FilePath
  , probeRtInSysPath
  , DlnaMode
  , dlnaIniEntries
  , extractDlnaIniEntries
  )
where

import           Control.Monad.Trans.Resource   ( MonadResource )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad                  ( when
                                                , join
                                                )
import           Control.Applicative            ( (<|>) )
import           Data.Monoid                    ( (<>)
                                                , All(..)
                                                , getAll
                                                )
import qualified Data.Conduit.Combinators      as CC
import           Data.Conduit                   ( (.|)
                                                , handleC
                                                , yield
                                                , ConduitT
                                                )
import           Conduit                        ( MonadUnliftIO )
import qualified Data.Text                     as T
import           Data.Text                      ( unpack
                                                , pack
                                                , Text
                                                , breakOn
                                                )
import           Data.Maybe                     ( fromMaybe
                                                , maybeToList
                                                )
import           Control.Exception              ( IOException
                                                , try
                                                )
import           Data.String.Utils              ( startswith )
import           System.Log.Logger              ( infoM )
import           System.FilePath                ( takeExtension
                                                , (<.>)
                                                , (</>)
                                                , takeFileName
                                                , dropExtension
                                                , takeDirectory
                                                )
import           System.Directory               ( doesFileExist
                                                , findExecutable
                                                )
import           System.Process                 ( callProcess )
import           System.IO                      ( Handle
                                                , hPutStr
                                                , hClose
                                                )
import qualified Data.ByteString.Lazy          as B
import           Data.Foldable                  ( find )
import qualified System.IO.Temp                as IOTemp
import           Control.Monad.Trans.Except     ( ExceptT(..)
                                                , runExceptT
                                                , withExceptT
                                                )
import qualified Data.HashMap.Lazy             as HashMap
import           Data.Ini                       ( Ini(..)
                                                , readIniFile
                                                )

newtype RootSourceDir = RootSourceDir FilePath deriving (Show, Eq)

newtype RootTargetDir = RootTargetDir FilePath deriving (Show, Eq)

type SourceFilePath = FilePath

type TargetDirPath = FilePath

newtype LoggerName = LoggerName String deriving (Show, Eq)

filePaths
  :: (MonadResource m, MonadUnliftIO m)
  => LoggerName
  -> FilePath
  -> ConduitT () FilePath m ()
filePaths (LoggerName loggerName) =
  errorHandled . (CC.sourceDirectoryDeep False)
 where
  errorHandled conduit =
    let eitherConduit  = CC.map Right
        maybeConduit   = CC.map (const Nothing `either` Just)
        filteredBySome = CC.concatMap id
        logConduit =
          let msg exception = "Could not access file: " <> show exception
              printMsg  = liftIO . infoM loggerName . msg
              doNothing = const . return $ ()
          in  CC.iterM $ printMsg `either` doNothing
        catched = handleC (\e -> yield (Left (e :: IOException)))
    in  catched (conduit .| eitherConduit)
        .| logConduit
        .| maybeConduit
        .| filteredBySome

cr2Paths
  :: (MonadResource m, MonadUnliftIO m)
  => LoggerName
  -> FilePath
  -> ConduitT () FilePath m ()
cr2Paths ln fp = filePaths ln fp .| CC.filter ((== ".CR2") . takeExtension)

data GetTargetDirectoryException = SourceFilePathIsNotUnderRootSourceDir RootSourceDir SourceFilePath
  deriving (Show, Eq)

getTargetDirectoryPath
  :: RootSourceDir
  -> RootTargetDir
  -> SourceFilePath
  -> Either GetTargetDirectoryException TargetDirPath
getTargetDirectoryPath (RootSourceDir rootSourceDir) (RootTargetDir rootTargetDir) sourceFilePath
  | rootSourceDir `startswith` sourceFilePath
  = Right . takeDirectory . unpack $ replaceOne (pack rootSourceDir)
                                                (pack rootTargetDir)
                                                (pack sourceFilePath)
  | otherwise
  = Left $ SourceFilePathIsNotUnderRootSourceDir (RootSourceDir rootSourceDir)
                                                 sourceFilePath
 where
  replaceOne :: Text -> Text -> Text -> Text
  replaceOne pattern substitution text
    | T.null back = text
    |    -- pattern doesn't occur
      otherwise = T.concat [front, substitution, T.drop (T.length pattern) back]
    where (front, back) = breakOn pattern text

isConversionNecessary
  :: SourceFilePath -> TargetDirPath -> Maybe PP3FilePath -> DlnaMode -> IO Bool
isConversionNecessary sourceFilePath targetDirPath maybeDefaultPp3FilePath dlnaMode
  = let
      targetFilePathExists =
        let targetFilePath = buildTargetFilePath
        in  assertFileExists targetFilePath
                             (logTargetFilePathDoesNotExistMsg targetFilePath)
      targetPp3FilePathExists =
        let
          targetPp3FilePath            = buildTargetPp3FilePath
          alternativeTargetPp3FilePath = buildAlternativeTargetPp3FilePath
          assertFileExists' p =
            assertFileExists p (logTargetPp3FilePathDoesNotExistMsg p)
          leftMap f e = withExceptT f e

          result' =
            leftMap All (assertFileExists' targetPp3FilePath)
              <|> leftMap All (assertFileExists' alternativeTargetPp3FilePath)
        in
          leftMap getAll result'
      targetPp3FileEqualsGivenPp3File maybeSourcePp3FilePath =
        getAll
          .          foldMap All
          <$>        flip equalsTargetPp3FilePath dlnaMode
          `traverse` (maybeSourcePp3FilePath <|> maybeDefaultPp3FilePath)
      result = do
        _ <- targetFilePathExists
        _ <- targetPp3FilePathExists
        targetPp3FileEqualsGivenPp3File' <-
          liftIO $ sourcePp3FilePath >>= targetPp3FileEqualsGivenPp3File
        pure $ not targetPp3FileEqualsGivenPp3File'
    in
      (id `either` id) <$> runExceptT result
 where
  buildTargetFilePath :: TargetFilePath
  buildTargetFilePath =
    targetDirPath </> dropExtension (takeFileName sourceFilePath) <.> "jpg"

  buildTargetPp3FilePath :: TargetFilePath
  buildTargetPp3FilePath = toPp3FilePath buildTargetFilePath

  buildAlternativeTargetPp3FilePath :: TargetFilePath
  buildAlternativeTargetPp3FilePath =
    toAlternativePp3FilePath buildTargetFilePath

  sourcePp3FilePath :: IO (Maybe PP3FilePath)
  sourcePp3FilePath =
    let p = toPp3FilePath sourceFilePath
    in  (\doesExist -> if doesExist then Just p else Nothing)
          <$> doesFileExist p

  assertFileExists :: FilePath -> IO x -> ExceptT Bool IO Bool
  assertFileExists fp ifNot =
    let handler fileExists' =
          if fileExists' then Right True else Left $ True <$ ifNot
    in  ExceptT . joinLeftSide $ handler <$> doesFileExist fp
   where
    joinLeftSide :: IO (Either (IO a) b) -> IO (Either a b)
    joinLeftSide io = io >>= ((Left <$>) `either` (pure . Right))

  equalsTargetPp3FilePath :: PP3FilePath -> DlnaMode -> IO Bool
  equalsTargetPp3FilePath sourcePp3 dlnaMode'
    = let
        targetPp3            = buildTargetPp3FilePath
        alternativeTargetPp3 = buildAlternativeTargetPp3FilePath
        targetPp3s           = [targetPp3, alternativeTargetPp3]
        result               = do
          targetPp3ExistsResult <- liftIO $ doesFileExist `traverse` targetPp3s
          let
            (targetPp3Exists, targetPp3') =
              let
                existMaybe =
                  ((== True) . fst)
                    `find` (targetPp3ExistsResult `zip` targetPp3s)
              in  (False, targetPp3) `fromMaybe` existMaybe
          _ <- if targetPp3Exists
            then ExceptT . pure $ Right ()
            else
              ExceptT
              $  Left ()
              <$ logTargetPp3FilePathDoesNotExistMsg targetPp3'
          result' <- liftIO $ contentEquals sourcePp3 targetPp3' dlnaMode'
          _       <-
            when (not result')
            . liftIO
            . putStrLn
            $ (  "source pp3 file "
              <> em sourcePp3
              <> " does not equal "
              <> em targetPp3'
              )
          pure result'
      in
        (const False `either` id) <$> runExceptT result
   where
    contentEquals :: FilePath -> FilePath -> DlnaMode -> IO Bool
    contentEquals fp1 fp2 False = contentEquals' B.readFile fp1 fp2
    contentEquals fp1 fp2 True  = contentEquals'
      ( (>>= either (\error' -> fail (show error')) (pure . setDlnaInitEntries))
      . readIniFile
      )
      fp1
      fp2

    contentEquals' :: (Applicative m, Eq b) => (a -> m b) -> a -> a -> m Bool
    contentEquals' readAction fp1' fp2' =
      let isEqual x = case x of
            [a, b] -> a == b
            _      -> False
          readResults = readAction `traverse` [fp1', fp2']
      in  fmap isEqual readResults
      {-let filtered t = let containsAppVersion t' = case LT.breakOn "AppVersion" t' of (_, "") -> True-}
                                                                                      {-_       -> False-}
                       {-in containsAppVersion `filter` LT.lines t-}
      {-in do-}
        {-[fp1T, fp2T] <- LTIO.readFile `traverse` [fp1, fp2]-}
        {-pure $ filtered fp1T == filtered fp2T-}
    -- this is enough when build problem with "AppVersion" key has been fixed

  logTargetFilePathDoesNotExistMsg :: TargetFilePath -> IO ()
  logTargetFilePathDoesNotExistMsg targetFilePath =
    putStrLn
      $  "Target file path "
      <> em targetFilePath
      <> " does not exist for source file "
      <> em sourceFilePath

  logTargetPp3FilePathDoesNotExistMsg :: PP3FilePath -> IO ()
  logTargetPp3FilePathDoesNotExistMsg targetPp3FilePath =
    putStrLn
      $  "Target pp3 file path "
      <> em targetPp3FilePath
      <> " does not exist for source file "
      <> em sourceFilePath

em :: String -> String
em s = "'" <> s <> "'"

determinePp3FilePath :: SourceFilePath -> IO (Maybe PP3FilePath)
determinePp3FilePath sourceFilePath =
  let pp3FilePath = toPp3FilePath sourceFilePath
  in  do
        doesFileExist' <- doesFileExist pp3FilePath
        pure $ if doesFileExist' then Just pp3FilePath else Nothing

toPp3FilePath :: FilePath -> PP3FilePath
toPp3FilePath fp = fp <.> "pp3"

toAlternativePp3FilePath :: FilePath -> PP3FilePath
toAlternativePp3FilePath fp = fp <.> "out" <.> "pp3"

type CR2FilePath = FilePath
type PP3FilePath = FilePath

findPp3 :: CR2FilePath -> IO (Maybe PP3FilePath)
findPp3 cr2 =
  let pp3 = cr2 <.> "pp3"
      toMaybe p = if p then Just pp3 else Nothing
  in  toMaybe <$> doesFileExist pp3

type RTExec = FilePath
type TargetFilePath = FilePath

type DlnaMode = Bool

execRT
  :: RTExec
  -> CR2FilePath
  -> PP3FilePath
  -> TargetFilePath
  -> DlnaMode
  -> IO (Either IOException ())
execRT executable cr2Path pp3Path targetFilePath dlnaMode =
  execRT' executable cr2Path (Just pp3Path) targetFilePath dlnaMode

probeRtInSysPath :: IO (Maybe FilePath)
probeRtInSysPath = findExecutable "rawtherapee"

execRTWithoutPp3
  :: RTExec
  -> CR2FilePath
  -> TargetFilePath
  -> DlnaMode
  -> IO (Either IOException ())
execRTWithoutPp3 executable cr2Path targetFilePath dlnaMode =
  execRT' executable cr2Path Nothing targetFilePath dlnaMode

dlnaIniEntries :: [(Text, Text)]
dlnaIniEntries =
  [ ("Enabled"  , "true")
  , ("Scale"    , "1")
  , ("AppliesTo", "Full image")
  , ("Method"   , "Lanczos")
  , ("Width"    , "4096")
  , ("Height"   , "4096")
  ]

extractDlnaIniEntries :: Ini -> [(Text, Text)]
extractDlnaIniEntries (Ini sections _) =
  let maybeDlnaIniEntries = HashMap.lookup "Resize" sections
  in  join . maybeToList $ maybeDlnaIniEntries

setDlnaInitEntries :: Ini -> Ini
setDlnaInitEntries (Ini sections globals) = Ini (setResizeSection sections)
                                                globals
  where setResizeSection = HashMap.adjust (const dlnaIniEntries) "Resize"

-- private

execRT'
  :: RTExec
  -> CR2FilePath
  -> Maybe PP3FilePath
  -> TargetFilePath
  -> DlnaMode
  -> IO (Either IOException ())
execRT' executable cr2Path pp3Path targetFilePath dlnaMode =
  let params dlnaPp3FilePath = toRtCliOptionList $ RtCliOptions
        { rcoCr2FilePath     = cr2Path
        , rcoTargetFilePath  = targetFilePath
        , rcoPp3FilePath     = pp3Path
        , rcoDlnaPp3FilePath = dlnaPp3FilePath
        }
      callProcess'' tempFilePath tempHandle = do
        params' <- if dlnaMode
          then writeDlnaFile tempHandle *> pure (params (Just tempFilePath))
          else pure (params Nothing)
        callProcess' executable params'
  in  IOTemp.withSystemTempFile "rawtherapee-convert-dlna-mode-pp3"
                                callProcess''
 where
  writeDlnaFile :: Handle -> IO ()
  writeDlnaFile h =
    let a = unlines
          [ "[Resize]"
          , "Enabled=true"
          , "Scale=1.0"
          , "AppliesTo=Full image"
          , "Method=Lanczos"
          , "DataSpecified=3"
          , "Width=4096"
          , "Height=4096"
          ]
    in  hPutStr h a *> hClose h

data RtCliOptions = RtCliOptions {
  rcoCr2FilePath :: String
, rcoTargetFilePath :: String
, rcoPp3FilePath :: Maybe String
, rcoDlnaPp3FilePath :: Maybe String
}

toRtCliOptionList :: RtCliOptions -> [String]
toRtCliOptionList opts =
  ["-O", rcoTargetFilePath opts, "-Y"]
    <>          ["-d"]
    `fromMaybe` (("-p" :) . pure <$> rcoPp3FilePath opts)
    <>          []
    `fromMaybe` (("-p" :) . pure <$> rcoDlnaPp3FilePath opts)
    <>          ["-c", rcoCr2FilePath opts]

callProcess' :: String -> [String] -> IO (Either IOException ())
callProcess' executable args = try $ callProcess executable args
