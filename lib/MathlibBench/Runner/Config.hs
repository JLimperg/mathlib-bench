{-# LANGUAGE OverloadedStrings #-}

module MathlibBench.Runner.Config where

import           System.FilePath ((</>))

_LEANPKG :: FilePath
_LEANPKG = "/root/.elan/bin/leanpkg"

_NUM_THREADS :: Int
_NUM_THREADS = 4

_MEM_LIMIT_MB :: Int
_MEM_LIMIT_MB = 20000

_SUPERVISOR_BASE_URL :: String
_SUPERVISOR_BASE_URL = "http://mathlib-bench.limperg.de"

_NEXT_COMMIT_URL :: String
_NEXT_COMMIT_URL = _SUPERVISOR_BASE_URL ++ "/next"

_FINISHED_URL :: String
_FINISHED_URL = _SUPERVISOR_BASE_URL ++ "/finished"

_ROOTDIR :: FilePath
_ROOTDIR = "/mathlib-bench/runner"

_WORKDIR :: FilePath
_WORKDIR = _ROOTDIR </> "work"

_ERROR_DELAY_SEC :: Int
_ERROR_DELAY_SEC = 30
