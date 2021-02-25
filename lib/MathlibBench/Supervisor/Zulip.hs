{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MathlibBench.Supervisor.Zulip
( Auth(..)
, MessageDestination(..)
, MessageMetadata(..)
, TimingMessage(..)
, postTimingMessage )
where

import           Control.Monad (void)
import           Data.Bifunctor (bimap)
import           Data.ByteString (ByteString)
import           Data.Function ((&))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import           Data.Time (NominalDiffTime, formatTime, defaultTimeLocale)
import           Network.HTTP.Simple
import           Network.HTTP.Types (urlEncode)

import           MathlibBench.Types
import           MathlibBench.Logging (logInfo)

data Auth = Auth
  { zulipInstance :: Text
  , user :: Text
  , password :: Text
  }

data MessageDestination = MessageDestination
  { stream :: Text
  , topic :: Text
  }

data MessageMetadata = MessageMetadata
  { auth :: Auth
  , destination :: MessageDestination
  }

data TimingMessage = TimingMessage
  { commit :: CommitHash
  , elapsed :: NominalDiffTime
  }

setRequestBodyURLEncodedText :: [(Text, Text)] -> Request -> Request
setRequestBodyURLEncodedText params
  = setRequestBodyURLEncoded $ map (bimap T.encodeUtf8 T.encodeUtf8) params

postMessageRequest :: Auth -> MessageDestination -> Text -> IO Request
postMessageRequest Auth {..} MessageDestination {..} content = do
  let dat =
        [ ("type", "stream")
        , ("to", stream)
        , ("subject", topic)
        , ("content", content)
        ]
  req <- parseRequest $ mconcat
    ["POST https://", T.unpack zulipInstance, ".zulipchat.com/api/v1/messages"]
  pure $ req
    & setRequestBodyURLEncodedText dat
    & setRequestBasicAuth (T.encodeUtf8 user) (T.encodeUtf8 password)

postMessage :: MessageMetadata -> Text -> IO ()
postMessage MessageMetadata { auth, destination } content = do
  req <- postMessageRequest auth destination content
  logInfo $ mconcat
    [ "posting timing message to "
    , TL.fromStrict $ zulipInstance auth
    , " Zulip, stream "
    , TL.fromStrict $ stream destination
    , ", topic "
    , TL.fromStrict $ topic destination
    ]
  void $ httpNoBody req

renderTimingMessage :: TimingMessage -> Text
renderTimingMessage TimingMessage { commit, elapsed } = mconcat
  [ "Build for commit ["
  , commitHash
  , "](https://mathlib-bench.limperg.de/commit/"
  , commitHash
  , ") finished in "
  , T.pack $ formatTime defaultTimeLocale "%Hh%Mm%Ss" elapsed
  ]
  where
    commitHash = fromCommitHash commit

postTimingMessage :: MessageMetadata -> TimingMessage -> IO ()
postTimingMessage meta = postMessage meta . renderTimingMessage
