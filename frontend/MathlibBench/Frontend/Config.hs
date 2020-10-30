module MathlibBench.Frontend.Config where

import Data.Fixed (Centi)

_DIFF_BASE_URL :: String
_DIFF_BASE_URL = "https://github.com/leanprover-community/mathlib/compare"

_COMMIT_BASE_URL :: String
_COMMIT_BASE_URL = "https://github.com/leanprover-community/mathlib/commit"

_EXPECTED_TIME_DIFF_PERCENT_VARIATION :: Centi
_EXPECTED_TIME_DIFF_PERCENT_VARIATION = 3
