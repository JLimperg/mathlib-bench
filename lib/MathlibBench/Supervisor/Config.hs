module MathlibBench.Supervisor.Config where

import           Data.Fixed (Centi)
import           Data.Int (Int64)
import           System.FilePath ((</>))

import           MathlibBench.UnixSeconds

_ROOTDIR :: FilePath
_ROOTDIR = "/mathlib-bench/supervisor"

_WORKDIR :: FilePath
_WORKDIR = _ROOTDIR </> "work"

_SQLITE_FILE :: FilePath
_SQLITE_FILE = _ROOTDIR </> "db.sqlite"

_DIFF_BASE_URL :: String
_DIFF_BASE_URL = "https://github.com/leanprover-community/mathlib/compare"

_COMMIT_BASE_URL :: String
_COMMIT_BASE_URL = "https://github.com/leanprover-community/mathlib/commit"

_EXPECTED_TIME_DIFF_PERCENT_VARIATION :: Centi
_EXPECTED_TIME_DIFF_PERCENT_VARIATION = 3

_GIT_REPO_CACHE_DURATION_SECONDS :: Int64
_GIT_REPO_CACHE_DURATION_SECONDS = 60

_TIMING_TIMEOUT :: UnixSeconds
_TIMING_TIMEOUT = 2*60*60

_PORT :: Int
_PORT = 8080
