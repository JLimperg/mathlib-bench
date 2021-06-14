{-# LANGUAGE OverloadedStrings #-}

module MathlibBench.Runner.Config where

import           System.FilePath ((</>))

_ELAN_BINDIR :: FilePath
_ELAN_BINDIR = "/root/.elan/bin"

_LEANPKG :: FilePath
_LEANPKG = _ELAN_BINDIR </> "leanpkg"

_LEAN :: FilePath
_LEAN = _ELAN_BINDIR </> "lean"

_NUM_THREADS :: Int
_NUM_THREADS = 4

_MEM_LIMIT_MB :: Int
_MEM_LIMIT_MB = 20000

_NEXT_COMMIT_URL_SUFFIX :: String
_NEXT_COMMIT_URL_SUFFIX = "/next"

_FINISHED_URL_SUFFIX :: String
_FINISHED_URL_SUFFIX = "/finished"

_ROOTDIR :: FilePath
_ROOTDIR = "/mathlib-bench/runner"

_WORKDIR :: FilePath
_WORKDIR = _ROOTDIR </> "work"

_ERROR_DELAY_SEC :: Int
_ERROR_DELAY_SEC = 30
