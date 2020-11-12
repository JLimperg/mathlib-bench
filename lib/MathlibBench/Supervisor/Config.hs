{-# LANGUAGE OverloadedStrings#-}

module MathlibBench.Supervisor.Config where

import           Data.Fixed (Centi)
import           Data.Int (Int64)
import           Data.Time (NominalDiffTime)
import           Database.PostgreSQL.Simple (ConnectInfo(..))
import           System.FilePath ((</>))

_ROOTDIR :: FilePath
_ROOTDIR = "/mathlib-bench/supervisor"

_WORKDIR :: FilePath
_WORKDIR = _ROOTDIR </> "work"

_DATABASE_CONNECTION_INFO :: ConnectInfo
_DATABASE_CONNECTION_INFO = ConnectInfo
  { connectHost = "localhost"
  , connectPort = 5432
  , connectUser = "mathlib_bench"
  , connectDatabase = "mathlib_bench"
  , connectPassword = ""
  }

_DIFF_BASE_URL :: String
_DIFF_BASE_URL = "https://github.com/leanprover-community/mathlib/compare"

_COMMIT_BASE_URL :: String
_COMMIT_BASE_URL = "https://github.com/leanprover-community/mathlib/commit"

_EXPECTED_TIME_DIFF_PERCENT_VARIATION :: Centi
_EXPECTED_TIME_DIFF_PERCENT_VARIATION = 3

_GIT_REPO_CACHE_DURATION_SECONDS :: Int64
_GIT_REPO_CACHE_DURATION_SECONDS = 60

_TIMING_TIMEOUT :: NominalDiffTime
_TIMING_TIMEOUT = 2*60*60

_DEFAULT_PORT :: Int
_DEFAULT_PORT = 8080
