{-# LANGUAGE OverloadedStrings#-}

module MathlibBench.Runner.Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Exception (handle)
import           Control.Monad (void, forever)
import qualified Data.Text.Lazy as TL
import           Data.Time (defaultTimeLocale, formatTime)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Network.HTTP.Simple
  (HttpException,  setRequestBodyJSON, getResponseBody, httpJSON, httpNoBody )
import           System.Directory (withCurrentDirectory)

import qualified MathlibBench.Api as Api
import           MathlibBench.Command
import           MathlibBench.GitRepo (GitRepoLock)
import qualified MathlibBench.GitRepo as Git
import           MathlibBench.Logging
import           MathlibBench.Runner.Config
import           MathlibBench.Types

getNextCommit :: IO Api.NextCommit
getNextCommit = getResponseBody <$> httpJSON _NEXT_COMMIT_REQUEST

reportTiming :: Api.FinishedTiming -> IO ()
reportTiming timing
  = void $ httpNoBody $ setRequestBodyJSON timing _FINISHED_REQUEST

timeBuild :: GitRepoLock -> CommitHash -> IO ElapsedTimeMillis
timeBuild lock commit =
  Git.withGitRepoLock lock $ \locked -> do
    Git.pull _WORKDIR locked
    Git.checkoutCleanCommit _WORKDIR locked commit
    startTime <- getPOSIXTime
    logInfo $
      "starting build for commit " <> TL.fromStrict (fromCommitHash commit)
    withCurrentDirectory _WORKDIR $
      cmd_ _LEANPKG
        [ "build", "--", "--threads", show _NUM_THREADS, "--memory"
        , show _MEM_LIMIT_MB ]
    endTime <- getPOSIXTime
    let timeDiff = endTime - startTime
    logInfo $ "build finished in " <>
      TL.pack (formatTime defaultTimeLocale "%Hh%Mm%Ss" timeDiff)
    pure $ nominalDiffTimeToElapsedTimeMillis timeDiff

main :: IO ()
main = do
  setupLogging
  Git.setupGitRepo _WORKDIR
  lock <- Git.newGitRepoLock
  forever $ handle exceptionHandler $ do
    nextCommit <- getNextCommit
    case nextCommit of
      Api.NoNextCommit -> pure ()
      Api.NextCommit commit inProgressId -> do
        elapsed <- timeBuild lock commit
        reportTiming $ Api.FinishedTiming commit elapsed inProgressId
  where
    exceptionHandler :: HttpException -> IO ()
    exceptionHandler e = do
      logError "error while communicating with the supervisor:"
      logError $ TL.pack $ show e
      logInfo $
        "sleeping " <> TL.pack (show _ERROR_DELAY_SEC) <>
        "s due to previous error"
      threadDelay $ _ERROR_DELAY_SEC * 1000000