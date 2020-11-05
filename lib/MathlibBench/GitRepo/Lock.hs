module MathlibBench.GitRepo.Lock
( GitRepoLock
, GitRepoLocked
, newGitRepoLock
, withGitRepoLock
) where

import           Control.Concurrent.Lock (Lock)
import qualified Control.Concurrent.Lock as Lock

newtype GitRepoLock = GitRepoLock { _fromGitRepoLock :: Lock }

data GitRepoLocked = GitRepoLocked

newGitRepoLock :: IO GitRepoLock
newGitRepoLock = GitRepoLock <$> Lock.new

withGitRepoLock :: GitRepoLock -> (GitRepoLocked -> IO a) -> IO a
withGitRepoLock (GitRepoLock lock) f = Lock.with lock (f GitRepoLocked)
