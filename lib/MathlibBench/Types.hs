{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MathlibBench.Types where

import           Data.Aeson
import           Data.Text (Text)
import           Data.Time.Clock (NominalDiffTime)
import           Database.PostgreSQL.Simple.FromField (FromField(..))
import           Database.PostgreSQL.Simple.ToField (ToField(..))

newtype CommitHash = CommitHash { fromCommitHash :: Text }
  deriving (FromField, ToField, FromJSON, ToJSON)

newtype ElapsedTimeMillis = ElapsedTimeMillis { fromElapsedTimeMillis :: Int }
  deriving (FromField, ToField, FromJSON, ToJSON)

nominalDiffTimeToElapsedTimeMillis :: NominalDiffTime -> ElapsedTimeMillis
nominalDiffTimeToElapsedTimeMillis = ElapsedTimeMillis . floor . (1e3 *)

elapsedTimeMillisToNominalDiffTime :: ElapsedTimeMillis -> NominalDiffTime
elapsedTimeMillisToNominalDiffTime = (/ 1e3) . fromIntegral . fromElapsedTimeMillis
