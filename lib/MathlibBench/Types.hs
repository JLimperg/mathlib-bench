module MathlibBench.Types where

import           Data.Aeson
import           Data.Text (Text)
import           Data.Time.Clock (NominalDiffTime)
import           Database.SQLite.Simple.FromField (FromField(..))
import           Database.SQLite.Simple.ToField (ToField(..))

newtype CommitHash = CommitHash { fromCommitHash :: Text }

instance FromJSON CommitHash where
  parseJSON = fmap CommitHash . parseJSON

instance ToJSON CommitHash where
  toJSON = toJSON . fromCommitHash
  toEncoding = toEncoding . fromCommitHash

instance FromField CommitHash where
  fromField = fmap CommitHash . fromField

instance ToField CommitHash where
  toField = toField . fromCommitHash

newtype ElapsedTimeMillis = ElapsedTimeMillis { fromElapsedTimeMillis :: Int }

instance FromJSON ElapsedTimeMillis where
  parseJSON = fmap ElapsedTimeMillis . parseJSON

instance ToJSON ElapsedTimeMillis where
  toJSON = toJSON . fromElapsedTimeMillis
  toEncoding = toEncoding . fromElapsedTimeMillis

instance FromField ElapsedTimeMillis where
  fromField = fmap ElapsedTimeMillis . fromField

instance ToField ElapsedTimeMillis where
  toField = toField . fromElapsedTimeMillis

nominalDiffTimeToElapsedTimeMillis :: NominalDiffTime -> ElapsedTimeMillis
nominalDiffTimeToElapsedTimeMillis = ElapsedTimeMillis . floor . (1e3 *)

elapsedTimeMillisToNominalDiffTime :: ElapsedTimeMillis -> NominalDiffTime
elapsedTimeMillisToNominalDiffTime = (/ 1e3) . fromIntegral . fromElapsedTimeMillis
