{-# LANGUAGE OverloadedStrings #-}

module MathlibBench.Runner.Config where

import           Network.HTTP.Client.Conduit (Request(..), parseRequest_)
import           System.FilePath ((</>))

_LEANPKG :: FilePath
_LEANPKG = "/root/.elan/bin/leanpkg"

_NUM_THREADS :: Int
_NUM_THREADS = 4

_MEM_LIMIT_MB :: Int
_MEM_LIMIT_MB = 20000

_SUPERVISOR_BASE_URL :: String
_SUPERVISOR_BASE_URL = "http://mathlib-bench.limperg.de"

_NEXT_COMMIT_REQUEST :: Request
_NEXT_COMMIT_REQUEST
  = (parseRequest_ (_SUPERVISOR_BASE_URL ++ "/next"))
      { method = "POST" }

_FINISHED_REQUEST :: Request
_FINISHED_REQUEST
  = (parseRequest_ (_SUPERVISOR_BASE_URL ++ "/finished"))
      { method = "POST" }

_ROOTDIR :: FilePath
_ROOTDIR = "/mathlib-bench/runner"

_WORKDIR :: FilePath
_WORKDIR = _ROOTDIR </> "work"

_ERROR_DELAY_SEC :: Int
_ERROR_DELAY_SEC = 30
