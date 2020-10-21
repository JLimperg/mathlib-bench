module Config where

import System.FilePath ((</>))

_ROOTDIR :: FilePath
_ROOTDIR = "/home/mathlib-bench/root"

_WORKDIR :: FilePath
_WORKDIR = _ROOTDIR </> "work"

_SQLITE_FILE :: FilePath
_SQLITE_FILE = _ROOTDIR </> "db.sqlite"

_MATHLIB_GIT_URL :: String
_MATHLIB_GIT_URL = "https://github.com/leanprover-community/mathlib"

_LEANPKG :: FilePath
_LEANPKG = "/home/mathlib-bench/.elan/bin/leanpkg" -- TODO change

_NUM_THREADS :: Int
_NUM_THREADS = 2

_MEM_LIMIT_MB :: Int
_MEM_LIMIT_MB = 6000
