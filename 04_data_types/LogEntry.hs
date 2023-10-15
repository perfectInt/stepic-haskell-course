import Data.Time.Clock
import Data.Time.Format
import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info

data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String}

instance Show LogLevel where
	show Error = "Error"
	show Warning = "Warning"
	show Info = "Info"

logLevelToString :: LogLevel -> String
logLevelToString x = show x

logEntryToString :: LogEntry -> String
logEntryToString (LogEntry timestamp logLevel message) = 
	timeToString timestamp ++ ": " ++ logLevelToString logLevel ++ ": " ++ message