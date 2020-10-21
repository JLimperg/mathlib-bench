{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Monad (when, void, forever)
import qualified Data.ByteString.Lazy as BL
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Database.SQLite.Simple
  ( query, Only(..), execute, withConnection, execute_, field )
import           System.Directory
  ( doesDirectoryExist, withCurrentDirectory, createDirectoryIfMissing
  , removeDirectoryRecursive )
import           System.Exit (exitFailure, ExitCode(..))
import           System.Process.Typed (readProcess, proc)

import MathlibBench.Config
import MathlibBench.Types
import MathlibBench.Backend.Config
import MathlibBench.Backend.Logging

cmd :: String -> [String] -> IO (BL.ByteString, BL.ByteString)
cmd prog args = do
  (exitcode, stdout, stderr) <- readProcess $ proc prog args
  case exitcode of
    ExitSuccess -> pure (stdout, stderr)
    ExitFailure n -> do
      logError "external command failed"
      logError $ TL.unwords $ "command:" : TL.pack prog : map TL.pack args
      logError $ "exit code: " <> TL.pack (show n)
      logError "stdout:"
      TL.putStrLn $ TL.decodeUtf8 stdout
      logError "stderr:"
      TL.putStrLn $ TL.decodeUtf8 stderr
      exitFailure

cmd_ :: String -> [String] -> IO ()
cmd_ prog args = void $ cmd prog args

setupRootDir :: IO ()
setupRootDir = do
  logInfo $ "creating root directory " <> TL.pack _ROOTDIR
  createDirectoryIfMissing True _ROOTDIR

setupGitRepo :: IO ()
setupGitRepo = do
  workDirExists <- doesDirectoryExist _WORKDIR
  when workDirExists $ do
    logInfo $ "removing old work directory " <> TL.pack _WORKDIR
    removeDirectoryRecursive _WORKDIR
  logInfo $ "cloning mathlib in " <> TL.pack _WORKDIR
  cmd_ "git" ["clone", _MATHLIB_GIT_URL, _WORKDIR]

setupDb :: IO ()
setupDb = do
  logInfo $ "setting up database in " <> TL.pack _SQLITE_FILE
  withConnection _SQLITE_FILE $ \conn -> do
    execute_ conn $ fromString $ unwords
      [ "CREATE TABLE IF NOT EXISTS timings ("
      , "id INTEGER PRIMARY KEY,"
      , "commit_hash TEXT NOT NULL,"
      , "elapsed_millis INTEGER NOT NULL )" ]

getHeadCommit :: IO CommitHash
getHeadCommit = withCurrentDirectory _WORKDIR $ do
  cmd_ "git" ["pull"]
  (stdout, _) <- cmd "git" ["log", "-n1", "--format=format:%H"]
  let commit = T.decodeUtf8 $ BL.toStrict stdout
  logInfo $ "current HEAD: " <> TL.fromStrict commit
  pure $ CommitHash commit

hasTimingForCommit :: CommitHash -> IO Bool
hasTimingForCommit commit = do
  withConnection _SQLITE_FILE $ \conn -> do
    r <- query conn "SELECT id FROM timings WHERE commit_hash = ?" (Only commit) :: IO [Only Int]
    pure $ not $ null r

timeBuild :: CommitHash -> IO ElapsedTimeMillis
timeBuild commit = withCurrentDirectory _WORKDIR $ do
  logInfo $ "starting build for commit " <> TL.fromStrict (fromCommitHash commit)
  cmd_ "git" ["reset", "--hard", "HEAD"]
  cmd_ "git" ["clean", "-fdx"]
  cmd_ _LEANPKG ["configure"]
  startTime <- getPOSIXTime
  cmd_ _LEANPKG
    [ "build", "--", "--threads", show _NUM_THREADS, "--memory"
    , show _MEM_LIMIT_MB ]
  endTime <- getPOSIXTime
  logInfo "build finished"
  pure $ nominalDiffTimeToElapsedTimeMillis $ endTime - startTime

writeTiming :: CommitHash -> ElapsedTimeMillis -> IO ()
writeTiming commit time = withConnection _SQLITE_FILE $ \conn ->
  execute conn "INSERT INTO timings (commit_hash, elapsed_millis) VALUES (?, ?)" (commit, time)

main :: IO ()
main = do
  setupLogging
  logInfo "====== mathlib-bench starting ======"
  setupRootDir
  setupDb
  setupGitRepo
  forever $ do
    headCommit <- getHeadCommit
    hasTiming <- hasTimingForCommit headCommit
    if hasTiming
      then do
        logInfo "HEAD has already been built"
        threadDelay $ 5*60*1000000
      else do
        elapsed <- timeBuild headCommit
        writeTiming headCommit elapsed
