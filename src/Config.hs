module Config where

import System.FilePath ((</>))

_ROOTDIR :: FilePath
_ROOTDIR = "/mathlib-bench"

_WORKDIR :: FilePath
_WORKDIR = _ROOTDIR </> "work"

_SQLITE_FILE :: FilePath
_SQLITE_FILE = _ROOTDIR </> "db.sqlite"

_MATHLIB_GIT_URL :: String
_MATHLIB_GIT_URL = "https://github.com/JLimperg/mathlib-bench-test" -- TODO change

_LEANPKG :: FilePath
_LEANPKG = "/home/jannis/.elan/bin/leanpkg" -- TODO change
