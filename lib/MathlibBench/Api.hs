{-# LANGUAGE OverloadedStrings #-}

module MathlibBench.Api
( emptyPostRequest
, jsonPostRequest
, validateSecretHeader
, NextCommit(..)
, FinishedTiming(..)
) where

import           Data.Aeson
import           Data.Aeson.Types (unexpected)
import           Data.String (fromString)
import           Network.HTTP.Simple
  ( setRequestMethod,  Request, parseRequest_, setRequestBodyJSON
  , addRequestHeader )
import           Web.Scotty (ActionM)
import qualified Web.Scotty as Scotty

import           MathlibBench.Config (_SECRET_HEADER)
import           MathlibBench.Secret (Secret, secretToLazyText, fromSecret)
import           MathlibBench.Types (CommitHash, ElapsedTimeMillis)
import Control.Monad (unless)

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
