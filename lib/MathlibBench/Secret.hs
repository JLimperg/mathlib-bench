module MathlibBench.Secret
( Secret(fromSecret)
, randomSecret
, parseSecret
, secretToText
, secretToLazyText
) where

import           Control.Monad (guard)
import           Data.Char (ord)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Vector.Unboxed as V
import           Data.Word (Word8)
import           System.Random (RandomGen, randomR)

import           MathlibBench.Config

-- A secret is a string of printable ASCII chars excluding DEL (ASCII char 127).
-- This means it may contain characters between 32 and 126 (inclusive).
newtype Secret = Secret { fromSecret :: ByteString }
  deriving (Eq)

isValidSecretChar :: Char -> Bool
isValidSecretChar c = ord c >= 32 && ord c <= 126

randomCharFromSecretCharset :: RandomGen g => g -> (Word8, g)
randomCharFromSecretCharset gen =
  let (n, gen') = randomR (0, _SECRET_CHARSET_LENGTH - 1) gen in
  (_SECRET_CHARSET V.! n, gen')

randomSecret :: RandomGen g => g -> (Secret, g)
randomSecret gen =
  let (secret, Just gen') =
        BS.unfoldrN _DEFAULT_SECRET_LENGTH (Just . randomCharFromSecretCharset)
          gen
  in
  (Secret secret, gen')

parseSecret :: Text -> Maybe Secret
parseSecret t = do
  guard $ T.all isValidSecretChar t
  pure $ Secret $ T.encodeUtf8 t

secretToText :: Secret -> Text
secretToText = T.decodeUtf8 . fromSecret

secretToLazyText :: Secret -> TL.Text
secretToLazyText = TL.decodeUtf8 . BL.fromStrict . fromSecret
