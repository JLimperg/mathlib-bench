{-# LANGUAGE OverloadedStrings #-}

module MathlibBench.Api
( emptyPostRequest
, jsonPostRequest
, validateSecretHeader
, NextCommit(..)
, FinishedTiming(..)
, FinishedPerFileTiming(..)
) where

import           Control.Monad (unless)
import           Data.Aeson
import           Data.Aeson.Types (unexpected)
import           Data.Map (Map)
import           Data.String (fromString)
import           Data.Text (Text)
import           Data.Time (NominalDiffTime, UTCTime)
import           Network.HTTP.Simple
  ( setRequestMethod,  Request, parseRequest_, setRequestBodyJSON
  , addRequestHeader )
import           Web.Scotty (ActionM)
import qualified Web.Scotty as Scotty

import           MathlibBench.Config (_SECRET_HEADER)
import           MathlibBench.Secret (Secret, secretToLazyText, fromSecret)
import           MathlibBench.Types (CommitHash, LinesOfCode)

setSecretHeader :: Secret -> Request -> Request
setSecretHeader secret
  = addRequestHeader (fromString _SECRET_HEADER) (fromSecret secret)

emptyPostRequest :: Secret -> String -> Request
emptyPostRequest secret
  = setSecretHeader secret
  . setRequestMethod "POST"
  . parseRequest_

jsonPostRequest :: ToJSON a => Secret -> a -> String -> Request
jsonPostRequest secret body
  = setRequestBodyJSON body . emptyPostRequest secret

validateSecretHeader :: Secret -> ActionM ()
validateSecretHeader expected = do
  header <- Scotty.header $ fromString _SECRET_HEADER
  case header of
    Nothing ->
      Scotty.raise $ "missing secret header: " <> fromString _SECRET_HEADER
    (Just headerVal) ->
      unless (headerVal == secretToLazyText expected) $
        Scotty.raise "incorrect secret"

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

data FinishedPerFileTiming = FinishedPerFileTiming
  { finishedPerFileTimingElapsed :: NominalDiffTime
  , finishedPerFileTimingLinesOfCode :: LinesOfCode
  }

instance FromJSON FinishedPerFileTiming where
    parseJSON = withObject "FinishedPerFileTiming" $ \v -> FinishedPerFileTiming
      <$> v .: "elapsed"
      <*> v .: "linesOfCode"

instance ToJSON FinishedPerFileTiming where
  toJSON (FinishedPerFileTiming elapsed loc) = object
    [ ("elapsed", toJSON elapsed)
    , ("linesOfCode", toJSON loc) ]
  toEncoding (FinishedPerFileTiming elapsed loc) = pairs $
    "elapsed" .= elapsed <>
    "linesOfCode" .= loc

data FinishedTiming = FinishedTiming
  { finishedTimingCommit :: CommitHash
  , finishedTimingInProgressId :: Int
  , finishedTimingStartTime :: UTCTime
  , finishedTimingEndTime :: UTCTime
  , finishedTimingRunnerId :: Text
  , finishedTimingPerFileTimings :: Map Text FinishedPerFileTiming
  }

instance FromJSON FinishedTiming where
  parseJSON = withObject "FinishedTiming" $ \v -> FinishedTiming
    <$> v .: "commit"
    <*> v .: "inProgressId"
    <*> v .: "startTime"
    <*> v .: "endTime"
    <*> v .: "runnerId"
    <*> v .: "perFileTimings"

instance ToJSON FinishedTiming where
  toJSON (FinishedTiming commit inProgressId startTime endTime runnerId perFileTimings) = object
    [ ("commit", toJSON commit)
    , ("inProgressId", toJSON inProgressId)
    , ("startTime", toJSON startTime)
    , ("endTime", toJSON endTime)
    , ("runnerId", toJSON runnerId)
    , ("perFileTimings", toJSON perFileTimings) ]
  toEncoding (FinishedTiming commit inProgressId startTime endTime runnerId perFileTimings) = pairs $
    "commit" .= commit <>
    "inProgressId" .= inProgressId <>
    "startTime" .= startTime <>
    "endTime" .= endTime <>
    "runnerId" .= runnerId <>
    "perFileTimings" .= perFileTimings
