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
    <*> v .: "timingInProgressId"
  parseJSON v = unexpected v

instance ToJSON NextCommit where
  toJSON NoNextCommit = Null
  toJSON (NextCommit commit timingInProgressId) = object
    [ ("commit", toJSON commit)
    , ("timingInProgressId", toJSON timingInProgressId) ]
  toEncoding NoNextCommit =  undefined
  toEncoding (NextCommit commit timingInProgressId) = pairs $
    "commit" .= commit <>
    "timingInProgressId" .= timingInProgressId

data FinishedTiming = FinishedTiming
  { finishedTimingCommit :: CommitHash
  , finishedTimingElapsed :: ElapsedTimeMillis
  , finishedTimingInProgressId :: Int
  }

instance FromJSON FinishedTiming where
  parseJSON = withObject "FinishedTiming" $ \v -> FinishedTiming <$>
    v .: "commit" <*>
    v .: "elapsed" <*>
    v .: "inprogressId"

instance ToJSON FinishedTiming where
  toJSON (FinishedTiming commit elapsed inprogressId) = object
    [ ("commit", toJSON commit), ("elapsed", toJSON elapsed)
    , ("inprogressId", toJSON inprogressId) ]
  toEncoding (FinishedTiming commit elapsed inprogressId) = pairs $
    "commit" .= commit <> "elapsed" .= elapsed <> "inprogressId" .= inprogressId
