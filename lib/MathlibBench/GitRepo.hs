{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module MathlibBench.GitRepo
( setupGitRepo
, fetch
, checkoutCleanCommit
, getHeadCommit
, getAllCommits
, getCommitsFromHeadTo
, module MathlibBench.GitRepo.Lock
) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Time (UTCTime, zonedTimeToUTC)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import           System.Directory (doesDirectoryExist)
import           System.Exit (exitFailure)

import           MathlibBench.Command
import           MathlibBench.Config (_GITHUB_REPO)
import           MathlibBench.GitRepo.Lock
import           MathlibBench.Logging
import           MathlibBench.Types

gitInWorkdir_ :: FilePath -> GitRepoLocked -> [String] -> IO ()
gitInWorkdir_ workdir _ args = cmd_ "git" $ ["-C", workdir] ++ args

gitInWorkdir
  :: FilePath -> GitRepoLocked -> [String] -> IO (BL.ByteString, BL.ByteString)
gitInWorkdir workdir _ args = cmd "git" $ ["-C", workdir] ++ args

setupGitRepo :: FilePath -> IO ()
setupGitRepo workdir = do
  workDirExists <- doesDirectoryExist workdir
  if workDirExists
    then logInfo $ "work dir '" <> TL.pack workdir <> "' already exists"
    else do
      logInfo $ "cloning mathlib in " <> TL.pack workdir
      cmd_ "git" ["clone", _GITHUB_REPO, workdir]

fetch :: FilePath -> GitRepoLocked -> IO ()
fetch workdir locked = gitInWorkdir_ workdir locked ["fetch"]

checkoutCleanCommit :: FilePath -> GitRepoLocked -> CommitHash -> IO ()
checkoutCleanCommit workdir locked commit = do
  gitInWorkdir_ workdir locked
    ["reset", "--hard", T.unpack (fromCommitHash commit)]
  gitInWorkdir_ workdir locked ["clean", "-fdx"]

gitLogFormat :: String
gitLogFormat = "format:%H,%cI"

parseGitLogLine :: TL.Text -> Maybe (CommitHash, UTCTime)
parseGitLogLine line
  = case TL.splitOn "," line of
      [hash, time] -> do
        let hash' = CommitHash $ TL.toStrict hash
        time' <- zonedTimeToUTC <$> iso8601ParseM (TL.unpack time)
        pure (hash', time')
      _ -> Nothing

parseGitLog :: TL.Text -> Maybe [(CommitHash, UTCTime)]
parseGitLog = mapM parseGitLogLine . TL.lines

handleGitLogParseFailure :: TL.Text -> Maybe a -> IO a
handleGitLogParseFailure log = \case
  (Just a) -> pure a
  Nothing -> do
    logError $ "Unable to decode Git log output:\n" <> log
    exitFailure

getHeadCommit :: FilePath -> GitRepoLocked -> IO (CommitHash, UTCTime)
getHeadCommit workdir locked = do
  logInfo "getting current HEAD commit of origin/master"
  (stdout, _) <- gitInWorkdir workdir locked
    ["log", "-n1", "--format=" ++ gitLogFormat, "origin/master"]
  let stdoutT = TL.decodeUtf8 stdout
  (commit, commitTime) <-
    handleGitLogParseFailure stdoutT $ parseGitLogLine stdoutT
  logInfo $ "current HEAD: " <> TL.fromStrict (fromCommitHash commit)
  pure (commit, commitTime)

getCommits
  :: FilePath -> GitRepoLocked -> Maybe CommitHash -> IO [(CommitHash, UTCTime)]
getCommits workdir locked origin = do
  let commitRange = case origin of
        Nothing -> "origin/master"
        Just origin -> T.unpack (fromCommitHash origin) ++ "..origin/master"
  (stdout, _) <- gitInWorkdir workdir locked $
    ["log", "--format=" ++ gitLogFormat, "--reverse"] ++ [commitRange]
  let stdoutT = TL.decodeUtf8 stdout
  handleGitLogParseFailure stdoutT $ parseGitLog stdoutT

getAllCommits :: FilePath -> GitRepoLocked -> IO [(CommitHash, UTCTime)]
getAllCommits workdir locked = do
  logInfo "getting all commits on origin/master"
  commits <- getCommits workdir locked Nothing
  logInfo $ "found " <> TL.pack (show (length commits)) <> " new commits"
  pure commits

getCommitsFromHeadTo
  :: FilePath -> GitRepoLocked -> CommitHash -> IO [(CommitHash, UTCTime)]
getCommitsFromHeadTo workdir locked origin = do
  logInfo $ "getting commits from " <> TL.fromStrict origin' <> " to origin/master HEAD"
  newCommits <- getCommits workdir locked (Just origin)
  logInfo $ "found " <> TL.pack (show (length newCommits)) <> " new commits"
  pure newCommits
  where
    origin' = fromCommitHash origin
