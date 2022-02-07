{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MathlibBench.Supervisor.Frontend.Json
( PerFileTiming(..)
, Build(..)
) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Map (Map)
import           Data.Text (Text)
import           Data.Time (UTCTime, NominalDiffTime)

import           MathlibBench.Types

data PerFileTiming = PerFileTiming
  { perFileTimingElapsed :: NominalDiffTime
  , perFileTimingLinesOfCode :: Maybe LinesOfCode
  , perFileTimingElapsedPerLineOfCode :: Maybe NominalDiffTime
  }

maybeToKeyValue :: (KeyValue kv, Monoid kv, ToJSON a) => Text -> Maybe a -> kv
maybeToKeyValue label Nothing = mempty
maybeToKeyValue label (Just x) = label .= x

instance ToJSON PerFileTiming where
  toJSON PerFileTiming {..} = Object $
    "elapsed" .= perFileTimingElapsed <>
    maybeToKeyValue "linesOfCode" perFileTimingLinesOfCode <>
    maybeToKeyValue "elapsedPerLineOfCode" perFileTimingElapsedPerLineOfCode
  toEncoding PerFileTiming {..} = pairs $
    "elapsed" .= perFileTimingElapsed <>
    maybeToKeyValue "linesOfCode" perFileTimingLinesOfCode <>
    maybeToKeyValue "elapsedPerLineOfCode" perFileTimingElapsedPerLineOfCode

data Build = Build
  { buildCommitHash :: CommitHash
  , buildCommitTime :: UTCTime
  , buildRunnerId :: Text
  , buildStartTime :: UTCTime
  , buildEndTime :: UTCTime
  , buildPerFileTimings :: Map Text PerFileTiming
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
