{-# LANGUAGE OverloadedStrings#-}

module MathlibBench.Runner.Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Exception (handle)
import           Control.Monad (when, forM, forM_, void, forever)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
import           MathlibBench.Runner.LinesOfCode (countLinesOfCode)
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
  Git.fetch _WORKDIR locked
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

timeFile :: GitRepoLocked -> FilePath -> IO (Text, NominalDiffTime, LinesOfCode)
timeFile _ fileRelativeToSrc = do
  let file = "src" </> fileRelativeToSrc
  startTime <- getCurrentTime
  cmd_ _LEAN ["-j0", file]
  endTime <- getCurrentTime
  loc <- countLinesOfCode file
  pure (T.pack fileRelativeToSrc, diffUTCTime endTime startTime, loc)

timeFiles :: GitRepoLocked -> CommitHash -> IO (Map Text (NominalDiffTime, LinesOfCode))
timeFiles locked commit = do
  logInfo $ "starting per-file build for commit " <> commitT
  timings <- withCurrentDirectory _WORKDIR $ do
    leanFiles <- getDirectoryFiles "src" ["**/*.lean"]
    fmap Map.fromList $ forM leanFiles $ \file -> do
      (fileT, elapsed, loc) <- timeFile locked file
      pure (fileT, (elapsed, loc))
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
            (Map.map (uncurry Api.FinishedPerFileTiming) perFileTimings)
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
oneShotMain OneShotCmdArgs { oneShotCmdArgsRuns = 0 } = pure ()
oneShotMain (OneShotCmdArgs commit runs doPerFileBuild) = do
  Git.setupGitRepo _WORKDIR
  lock <- Git.newGitRepoLock
  let commitText = TL.fromStrict $ fromCommitHash commit
  forM_ [1 .. runs] $ \run -> do
    let runText = TL.pack $ show run
    logInfo $ mconcat
      [ "run ", runText, ": starting full build for commit ", commitText ]
    void $ Git.withGitRepoLock lock $ \locked -> timeBuild locked commit
    when doPerFileBuild $ do
      logInfo $ mconcat
        [ "run ", runText, ": starting per-file build for commit ", commitText ]
      fileTimings <- Git.withGitRepoLock lock $ \locked ->
        timeFiles locked commit
      forM_ (Map.toAscList fileTimings) $ \(file, (elapsed, loc)) ->
        T.putStrLn $
          file <> " " <>
          T.pack (formatTime defaultTimeLocale "%-2Es" elapsed) <> " " <>
          T.pack (show $ fromLinesOfCode loc)

main :: IO ()
main = do
  args <- parseCmdArgs
  setupLogging
  case args of
    Daemon args -> daemonMain args
    OneShot args -> oneShotMain args
