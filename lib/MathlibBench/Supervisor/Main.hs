{-# LANGUAGE OverloadedStrings #-}

module MathlibBench.Supervisor.Main (main) where

import qualified Data.Text.Lazy as TL
import           System.Directory (createDirectoryIfMissing)

import           MathlibBench.GitRepo (setupGitRepo, newGitRepoLock)
import           MathlibBench.Logging
import           MathlibBench.Supervisor.CmdArgs
import           MathlibBench.Supervisor.Config
  (_ROOTDIR, _WORKDIR, _DATABASE_CONNECTION_INFO )
import           MathlibBench.Supervisor.Db (ConnectInfo(..), withConnection, createDb)
import           MathlibBench.Supervisor.Frontend (frontendMain)
import           MathlibBench.Supervisor.GitRepo.Timestamp (newGitRepoTimestamp)

main :: IO ()
main = do
  (CmdArgs secret dbPassword port zulipInfo) <- parseCmdArgs
  let connInfo = _DATABASE_CONNECTION_INFO { connectPassword = dbPassword }
  setupLogging
  logInfo "====== mathlib-bench starting ======"
  logInfo $ "creating root directory " <> TL.pack _ROOTDIR
  createDirectoryIfMissing True _ROOTDIR
  withConnection connInfo createDb
  setupGitRepo _WORKDIR
  gitRepoLock <- newGitRepoLock
  gitRepoTimestamp <- newGitRepoTimestamp
  frontendMain gitRepoLock gitRepoTimestamp secret connInfo port zulipInfo
