{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MathlibBench.Types where

import           Data.Aeson
import           Data.Text (Text)
import           Database.PostgreSQL.Simple.FromField (FromField(..))
import           Database.PostgreSQL.Simple.ToField (ToField(..))

newtype CommitHash = CommitHash { fromCommitHash :: Text }
  deriving (FromField, ToField, FromJSON, ToJSON)
