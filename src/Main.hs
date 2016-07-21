module Main where

import System.Log.Logger (updateGlobalLogger, setLevel, Priority (DEBUG), addHandler)
import System.Log.Handler.Syslog (openlog, Option (PID), Facility (USER))

main :: IO ()
main = validateInputs >>= (logInputException `either` convert)

convert :: UserSettings -> IO ()
convert = undefined

logInputException :: InputException -> IO ()
logInputException = undefined

validateInputs :: IO (Either InputException UserSettings)
validateInputs = undefined

data InputException

data UserSettings = UserSettings {
  -- |Base dir we'll look for cr2 files
  usSourceDir :: FilePath
  -- |Target dir under which we'll mirror the 'sourceDir' structure to place the converted pictures
, usTargetDir :: FilePath
  -- |Default pp3 file we'll use for converting when there's no file specific one found
, usDefaultPp3 :: Maybe FilePath
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
