{-# LANGUAGE OverloadedStrings #-}

module MathlibBench.Supervisor.Main (main) where

import qualified Data.Text.Lazy as TL
import           System.Directory (createDirectoryIfMissing)

import           MathlibBench.GitRepo (setupGitRepo, newGitRepoLock)
import           MathlibBench.Logging
import           MathlibBench.Supervisor.CmdArgs
import           MathlibBench.Supervisor.Config (_ROOTDIR, _WORKDIR)
import           MathlibBench.Supervisor.Db (createDb)
import           MathlibBench.Supervisor.Frontend (frontendMain)
import           MathlibBench.Supervisor.GitRepo.Timestamp (newGitRepoTimestamp)

main :: IO ()
main = do
  (CmdArgs secret) <- parseCmdArgs
  setupLogging
  logInfo "====== mathlib-bench starting ======"
  logInfo $ "creating root directory " <> TL.pack _ROOTDIR
  createDirectoryIfMissing True _ROOTDIR
  createDb
  setupGitRepo _WORKDIR
  gitRepoLock <- newGitRepoLock
  gitRepoTimestamp <- newGitRepoTimestamp
  frontendMain gitRepoLock gitRepoTimestamp secret
