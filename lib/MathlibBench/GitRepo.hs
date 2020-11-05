{-# LANGUAGE OverloadedStrings #-}

module MathlibBench.GitRepo
( setupGitRepo
, pull
, checkoutCleanCommit
, getHeadCommit
, getAllCommits
, getCommitsFromHeadTo
, module MathlibBench.GitRepo.Lock
) where

import           Control.Monad (when)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           System.Directory (doesDirectoryExist, removeDirectoryRecursive)

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
  when workDirExists $ do
    logInfo $ "removing old work directory " <> TL.pack workdir
    removeDirectoryRecursive workdir
  logInfo $ "cloning mathlib in " <> TL.pack workdir
  cmd_ "git" ["clone", _GITHUB_REPO, workdir]

pull :: FilePath -> GitRepoLocked -> IO ()
pull workdir locked = gitInWorkdir_ workdir locked ["pull"]

checkoutCleanCommit :: FilePath -> GitRepoLocked -> CommitHash -> IO ()
checkoutCleanCommit workdir locked commit = do
  gitInWorkdir_ workdir locked
    ["reset", "--hard", T.unpack (fromCommitHash commit)]
  gitInWorkdir_ workdir locked ["clean", "-fdx"]

getHeadCommit :: FilePath -> GitRepoLocked -> IO CommitHash
getHeadCommit workdir locked = do
  logInfo "getting current HEAD commit"
  (stdout, _) <- gitInWorkdir workdir locked ["log", "-n1", "--format=format:%H"]
  let commit = T.decodeUtf8 $ BL.toStrict stdout
  logInfo $ "current HEAD: " <> TL.fromStrict commit
  pure $ CommitHash commit

getCommits :: FilePath -> GitRepoLocked -> Maybe CommitHash -> IO [CommitHash]
getCommits workdir locked origin = do
  let commitRange = case origin of
        Nothing -> []
        Just origin -> [T.unpack (fromCommitHash origin) ++ "..HEAD"]
  (stdout, _) <- gitInWorkdir workdir locked $
    ["log", "--format=format:%H", "--reverse"] ++ commitRange
  pure $ map (CommitHash . TL.toStrict) $ TL.lines $ TL.decodeUtf8 stdout

getAllCommits :: FilePath -> GitRepoLocked -> IO [CommitHash]
getAllCommits workdir locked = do
  logInfo "getting all commits on master branch"
  commits <- getCommits workdir locked Nothing
  logInfo $ "found " <> TL.pack (show (length commits)) <> " new commits"
  pure commits

getCommitsFromHeadTo :: FilePath -> GitRepoLocked -> CommitHash -> IO [CommitHash]
getCommitsFromHeadTo workdir locked origin = do
  logInfo $ "getting commits from " <> TL.fromStrict origin' <> " to HEAD"
  newCommits <- getCommits workdir locked (Just origin)
  logInfo $ "found " <> TL.pack (show (length newCommits)) <> " new commits"
  pure newCommits
  where
    origin' = fromCommitHash origin
