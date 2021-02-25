{-# LANGUAGE OverloadedStrings#-}

-- TODO
module MathlibBench.Runner.Main (main, timeFiles) where

import           Control.Concurrent (threadDelay)
import           Control.Exception (handle)
import           Control.Monad (forM_, void, forever)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Time
  ( NominalDiffTime,  UTCTime, defaultTimeLocale, formatTime, getCurrentTime
  , diffUTCTime )
import           Network.HTTP.Simple
  ( HttpException, getResponseBody, httpJSON, httpNoBody )
import           System.Directory (withCurrentDirectory)
import           System.FilePath ((</>))
import           System.FilePattern.Directory (getDirectoryFiles)

import qualified MathlibBench.Api as Api
import           MathlibBench.Command
import           MathlibBench.GitRepo (GitRepoLocked)
import qualified MathlibBench.GitRepo as Git
import           MathlibBench.Logging
import           MathlibBench.Runner.CmdArgs
import           MathlibBench.Runner.Config
import           MathlibBench.Secret (Secret)
import           MathlibBench.Types

getNextCommit :: String -> Secret -> IO Api.NextCommit
getNextCommit supervisorUrl secret = getResponseBody <$>
  httpJSON (Api.emptyPostRequest secret $ supervisorUrl <> _NEXT_COMMIT_URL_SUFFIX)

reportTiming :: String -> Secret -> Api.FinishedTiming -> IO ()
reportTiming supervisorUrl secret timing = void $ httpNoBody $
  Api.jsonPostRequest secret timing $ supervisorUrl <> _FINISHED_URL_SUFFIX

timeBuild :: GitRepoLocked -> CommitHash -> IO (UTCTime, UTCTime)
timeBuild locked commit = do
  Git.pull _WORKDIR locked
  Git.checkoutCleanCommit _WORKDIR locked commit
  logInfo $
    "starting build for commit " <> TL.fromStrict (fromCommitHash commit)
  startTime <- getCurrentTime
  withCurrentDirectory _WORKDIR $
    cmd_ _LEANPKG
      [ "build", "--", "--threads", show _NUM_THREADS, "--memory"
      , show _MEM_LIMIT_MB ]
  endTime <- getCurrentTime
  let timeDiff = diffUTCTime endTime startTime
  logInfo $ "build finished in " <>
    TL.pack (formatTime defaultTimeLocale "%Hh%Mm%Ss" timeDiff)
  pure (startTime, endTime)

timeFile :: GitRepoLocked -> FilePath -> IO (Text, NominalDiffTime)
timeFile _ file = do
  startTime <- getCurrentTime
  cmd_ _LEAN ["-j0", "src" </> file]
  endTime <- getCurrentTime
  pure (T.pack file, diffUTCTime endTime startTime)

timeFiles :: GitRepoLocked -> CommitHash -> IO (Map Text NominalDiffTime)
timeFiles locked commit = do
  logInfo $ "starting per-file build for commit " <> commitT
  timings <- withCurrentDirectory _WORKDIR $ do
    leanFiles <- getDirectoryFiles "src" ["**/*.lean"]
    Map.fromList <$> mapM (timeFile locked) leanFiles
  logInfo $ "finished per-file build for commit " <> commitT
  pure timings
  where
    commitT = TL.fromStrict $ fromCommitHash commit

daemonMain :: DaemonCmdArgs -> IO ()
daemonMain (DaemonCmdArgs runnerId supervisorUrl secret) = do
  Git.setupGitRepo _WORKDIR
  lock <- Git.newGitRepoLock
  forever $ handle exceptionHandler $ do
    nextCommit <- getNextCommit supervisorUrl secret
    case nextCommit of
      Api.NoNextCommit -> pure ()
      Api.NextCommit commit inProgressId -> do
        (startTime, endTime, perFileTimings) <-
          Git.withGitRepoLock lock $ \locked -> do
            (startTime, endTime) <- timeBuild locked commit
            perFileTimings <- timeFiles locked commit
            pure (startTime, endTime, perFileTimings)
        reportTiming supervisorUrl secret $
          Api.FinishedTiming commit inProgressId startTime endTime runnerId
            perFileTimings
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

oneShotMain :: OneShotCmdArgs -> IO ()
oneShotMain (OneShotCmdArgs _ 0) = pure ()
oneShotMain (OneShotCmdArgs commit runs) = do
  Git.setupGitRepo _WORKDIR
  lock <- Git.newGitRepoLock
  forM_ [1 .. runs] $ \run -> do
    let runText = TL.pack $ show run
    let commitText = TL.fromStrict $ fromCommitHash commit
    logInfo $ mconcat
      [ "starting run ", runText, " for commit ", commitText ]
    Git.withGitRepoLock lock $ \locked -> timeBuild locked commit

main :: IO ()
main = do
  args <- parseCmdArgs
  setupLogging
  case args of
    Daemon args -> daemonMain args
    OneShot args -> oneShotMain args
