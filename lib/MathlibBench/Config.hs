module MathlibBench.Config where

import           Data.Char (ord)
import           Data.Word (Word8)
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

_GITHUB_REPO :: String
_GITHUB_REPO = "https://github.com/leanprover-community/mathlib"

_DEFAULT_SECRET_LENGTH :: Int
_DEFAULT_SECRET_LENGTH = 32

-- When we generate a secret, we use only the 64 characters used by the BASE64
-- encoding.
_SECRET_CHARSET :: Vector Word8
_SECRET_CHARSET = V.fromList $ map (fromIntegral . ord) $
  ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+', '/']

_SECRET_CHARSET_LENGTH :: Int
_SECRET_CHARSET_LENGTH = V.length _SECRET_CHARSET

_SECRET_HEADER :: String
_SECRET_HEADER = "X-Mathlib-Bench-Secret"
