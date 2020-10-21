module MathlibBench.Types where

import           Data.Text (Text)
import           Data.Time.Clock (NominalDiffTime)
import           Database.SQLite.Simple (ToRow(..), FromRow(..), field)
import           Database.SQLite.Simple.FromField (FromField(..))
import           Database.SQLite.Simple.ToField (ToField(..))

newtype CommitHash = CommitHash { fromCommitHash :: Text }

instance FromField CommitHash where
  fromField = fmap CommitHash . fromField

instance ToField CommitHash where
  toField = toField . fromCommitHash

newtype ElapsedTimeMillis = ElapsedTimeMillis { fromElapsedTimeMillis :: Int }

instance FromField ElapsedTimeMillis where
  fromField = fmap ElapsedTimeMillis . fromField

instance ToField ElapsedTimeMillis where
  toField = toField . fromElapsedTimeMillis

nominalDiffTimeToElapsedTimeMillis :: NominalDiffTime -> ElapsedTimeMillis
nominalDiffTimeToElapsedTimeMillis = ElapsedTimeMillis . floor . (1e3 *)

elapsedTimeMillisToNominalDiffTime :: ElapsedTimeMillis -> NominalDiffTime
elapsedTimeMillisToNominalDiffTime = (/ 1e3) . fromIntegral . fromElapsedTimeMillis

data Timing = Timing
  { timingId :: Int
  , timingCommit :: Text
  , timingElapsed :: ElapsedTimeMillis
  }

instance FromRow Timing where
  fromRow = Timing <$> field <*> field <*> field

instance ToRow Timing where
  toRow (Timing id_ commit elapsed) = toRow (id_, commit, elapsed)
