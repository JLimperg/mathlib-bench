{-# LANGUAGE OverloadedStrings #-}

module MathlibBench.Api where

import           Data.Aeson
import           Data.Aeson.Types (unexpected)

import           MathlibBench.Types (CommitHash, ElapsedTimeMillis)

data NextCommit
  = NoNextCommit
  | NextCommit CommitHash Int

instance FromJSON NextCommit where
  parseJSON Null = pure NoNextCommit
  parseJSON (Object v) = NextCommit
    <$> v .: "commit"
    <*> v .: "inProgressId"
  parseJSON v = unexpected v

instance ToJSON NextCommit where
  toJSON NoNextCommit = Null
  toJSON (NextCommit commit inProgressId) = object
    [ ("commit", toJSON commit)
    , ("inProgressId", toJSON inProgressId) ]
  toEncoding NoNextCommit = toEncoding Null
  toEncoding (NextCommit commit inProgressId) = pairs $
    "commit" .= commit <>
    "inProgressId" .= inProgressId

data FinishedTiming = FinishedTiming
  { finishedTimingCommit :: CommitHash
  , finishedTimingElapsed :: ElapsedTimeMillis
  , finishedTimingInProgressId :: Int
  }

instance FromJSON FinishedTiming where
  parseJSON = withObject "FinishedTiming" $ \v -> FinishedTiming
    <$> v .: "commit"
    <*> v .: "elapsed"
    <*> v .: "inProgressId"

instance ToJSON FinishedTiming where
  toJSON (FinishedTiming commit elapsed inProgressId) = object
    [ ("commit", toJSON commit)
    , ("elapsed", toJSON elapsed)
    , ("inProgressId", toJSON inProgressId) ]
  toEncoding (FinishedTiming commit elapsed inProgressId) = pairs $
    "commit" .= commit <>
    "elapsed" .= elapsed <>
    "inProgressId" .= inProgressId
