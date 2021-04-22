{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MathlibBench.Supervisor.Frontend.Json
( Build(..)
) where

import           Data.Aeson
import           Data.Map (Map)
import           Data.Text (Text)
import           Data.Time (UTCTime, NominalDiffTime)

import           MathlibBench.Types

data Build = Build
  { buildCommitHash :: CommitHash
  , buildCommitTime :: UTCTime
  , buildRunnerId :: Text
  , buildStartTime :: UTCTime
  , buildEndTime :: UTCTime
  , buildPerFileTimings :: Map Text NominalDiffTime
  }

instance ToJSON Build where
  toJSON Build {..} = object
    [ "commit" .= buildCommitHash
    , "commitTime" .= buildCommitTime
    , "runnerId" .= buildRunnerId
    , "startTime" .= buildStartTime
    , "endTime" .= buildEndTime
    , "timings" .= buildPerFileTimings
    ]
  toEncoding Build {..} = pairs
    $  "commit" .= buildCommitHash
    <> "commitTime" .= buildCommitTime
    <> "runnerId" .= buildRunnerId
    <> "startTime" .= buildStartTime
    <> "endTime" .= buildEndTime
    <> "timings" .= buildPerFileTimings
