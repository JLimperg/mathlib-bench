module MathlibBench.Supervisor.GitRepo.Timestamp
( GitRepoTimestamp
, newGitRepoTimestamp
, updateGitRepoUnlessUpToDate
)
where

import           Control.Concurrent (swapMVar, newMVar, MVar, readMVar)
import           Control.Monad (void)
import           Data.Int (Int64)
import           Data.Time.Clock.System (SystemTime(..), getSystemTime)

import           MathlibBench.GitRepo.Lock (GitRepoLocked)
import           MathlibBench.Supervisor.Config
  ( _GIT_REPO_CACHE_DURATION_SECONDS )

newtype GitRepoTimestamp = GitRepoTimestamp
  { _fromGitRepoTimestamp :: MVar Int64 }

newGitRepoTimestamp :: IO GitRepoTimestamp
newGitRepoTimestamp = GitRepoTimestamp <$> newMVar 0

updateGitRepoUnlessUpToDate
  :: GitRepoLocked -> GitRepoTimestamp -> IO a -> IO (Maybe a)
updateGitRepoUnlessUpToDate _ (GitRepoTimestamp timestamp) f = do
  lastSecs <- readMVar timestamp
  (MkSystemTime currentSecs _) <- getSystemTime
  if currentSecs - lastSecs <= _GIT_REPO_CACHE_DURATION_SECONDS
    then pure Nothing
    else do
      r <- f
      void $ swapMVar timestamp currentSecs
      pure $ Just r
