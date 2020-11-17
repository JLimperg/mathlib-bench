{-# LANGUAGE OverloadedStrings#-}

module MathlibBench.Runner.Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Exception (handle)
import           Control.Monad (void, forever)
import qualified Data.Text.Lazy as TL
import           Data.Time
  ( UTCTime, defaultTimeLocale, formatTime, getCurrentTime, diffUTCTime )
import           Network.HTTP.Simple
  ( HttpException, getResponseBody, httpJSON, httpNoBody )
import           System.Directory (withCurrentDirectory)

import qualified MathlibBench.Api as Api
import           MathlibBench.Command
import           MathlibBench.GitRepo (GitRepoLock)
import qualified MathlibBench.GitRepo as Git
import           MathlibBench.Logging
import           MathlibBench.Runner.CmdArgs
import           MathlibBench.Runner.Config
import           MathlibBench.Secret (Secret)
import           MathlibBench.Types

getNextCommit :: Secret -> IO Api.NextCommit
getNextCommit secret = getResponseBody <$>
  httpJSON (Api.emptyPostRequest secret _NEXT_COMMIT_URL)

reportTiming :: Secret -> Api.FinishedTiming -> IO ()
reportTiming secret timing = void $ httpNoBody $
  Api.jsonPostRequest secret timing _FINISHED_URL

timeBuild :: GitRepoLock -> CommitHash -> IO (UTCTime, UTCTime)
timeBuild lock commit =
  Git.withGitRepoLock lock $ \locked -> do
    Git.pull _WORKDIR locked
    Git.checkoutCleanCommit _WORKDIR locked commit
    startTime <- getCurrentTime
    logInfo $
      "starting build for commit " <> TL.fromStrict (fromCommitHash commit)
    withCurrentDirectory _WORKDIR $
      cmd_ _LEANPKG
        [ "build", "--", "--threads", show _NUM_THREADS, "--memory"
        , show _MEM_LIMIT_MB ]
    endTime <- getCurrentTime
    let timeDiff = diffUTCTime endTime startTime
    logInfo $ "build finished in " <>
      TL.pack (formatTime defaultTimeLocale "%Hh%Mm%Ss" timeDiff)
    pure (startTime, endTime)

main :: IO ()
main = do
  (CmdArgs runnerId secret) <- parseCmdArgs
  setupLogging
  Git.setupGitRepo _WORKDIR
  lock <- Git.newGitRepoLock
  forever $ handle exceptionHandler $ do
    nextCommit <- getNextCommit secret
    case nextCommit of
      Api.NoNextCommit -> pure ()
      Api.NextCommit commit inProgressId -> do
        (startTime, endTime) <- timeBuild lock commit
        reportTiming secret $
          Api.FinishedTiming commit inProgressId startTime endTime runnerId
  where
    -- TODO catch JSONException as well?
    exceptionHandler :: HttpException -> IO ()
    exceptionHandler e = do
      logError "error while communicating with the supervisor:"
      logError $ TL.pack $ show e
      logInfo $
        "sleeping " <> TL.pack (show _ERROR_DELAY_SEC) <>
        "s due to previous error"
      threadDelay $ _ERROR_DELAY_SEC * 1000000
