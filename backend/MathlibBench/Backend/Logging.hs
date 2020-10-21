{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MathlibBench.Backend.Logging
( setupLogging
, LogSeverity(..)
, logMsg
, logInfo
, logError
) where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import           System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

setupLogging :: IO ()
setupLogging = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

data LogSeverity
  = LogError
  | LogInfo

logSeverityToPrefix :: LogSeverity -> TL.Text
logSeverityToPrefix = \case
  LogError -> "[ERROR]"
  LogInfo -> "[INFO]"

logMsg :: LogSeverity -> TL.Text -> IO ()
logMsg severity msg =
  TL.putStrLn $ logSeverityToPrefix severity <> " " <> msg

logInfo :: TL.Text -> IO ()
logInfo = logMsg LogInfo

logError :: TL.Text -> IO ()
logError = logMsg LogError
