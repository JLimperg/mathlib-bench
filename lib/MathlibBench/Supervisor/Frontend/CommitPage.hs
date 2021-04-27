{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module MathlibBench.Supervisor.Frontend.CommitPage
( Commit(..)
, renderCommit
) where

import           Prelude hiding (head)

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
  ( UTCTime, NominalDiffTime, formatTime, defaultTimeLocale, diffUTCTime )
import           Text.Blaze.Html5 hiding (map)
import           Text.Blaze.Html5.Attributes hiding (title)

import           MathlibBench.Supervisor.Config (_COMMIT_BASE_URL)
import           MathlibBench.Types

data Commit = Commit
  { commitHash :: CommitHash
  , commitTime :: UTCTime
  , commitRunnerId :: Text
  , commitStartTime :: UTCTime
  , commitEndTime :: UTCTime
  , perFileTimings :: [(Text, NominalDiffTime)]
  }

data PerFileTiming = PerFileTiming
  { perFileTimingFilename :: Text
  , perFileTimingElapsed :: NominalDiffTime
  , perFileTimingHighlight :: Bool
  }

quantile :: (Ord a) => Double -> NonEmpty a -> a
quantile q as
  = let count = length as in
    NonEmpty.sort as NonEmpty.!! floor (fromIntegral count * q)

mungePerFileTimings :: [(Text, NominalDiffTime)] -> [PerFileTiming]
mungePerFileTimings [] = []
mungePerFileTimings timings@(t : ts)
  = let topTenPercent = quantile 0.9 $ fmap snd (t :| ts) in
    map (go topTenPercent) timings
  where
    go :: NominalDiffTime -> (Text, NominalDiffTime) -> PerFileTiming
    go highlightTime (file, time) = PerFileTiming
      { perFileTimingFilename = file
      , perFileTimingElapsed = time
      , perFileTimingHighlight = time >= highlightTime
      }

formatTimestamp :: UTCTime -> String
formatTimestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z"

renderCommit :: Commit -> Html
renderCommit Commit { ..} = docTypeHtml $ do
  head $ do
    meta ! charset "UTF-8"
    link ! rel "stylesheet" ! href "/global.css"
    title $ text $ "Commit " <> commitText

  body $ do
    h1 $ text $ "Commit " <> commitText

    h2 "Commit Metadata"
    p $ do
      string $ "Commit time: " <> formatTimestamp commitTime
      br
      a ! href (stringValue $ _COMMIT_BASE_URL ++ "/" ++ commitString) $
        "View on Github"

    h2 "Full Build Timing"
    p $ do
      string $ "Elapsed time: " <>
        formatTime defaultTimeLocale "%Hh%Mm%Ss" fullBuildTime
      br
      string $ "Build started: " <> formatTimestamp commitStartTime
      br
      string $ "Build finished: " <> formatTimestamp commitEndTime
      br
      text $ "Runner: " <> commitRunnerId
      br
      a ! href (stringValue $ "/commit/" ++ commitString ++ "/json") $
        "Timing data as JSON"
      br
      a ! href
        (stringValue $
           "https://observablehq.com/d/30654ef5fe66acda?commit=%22" ++
             commitString ++ "%22#chart") $
        "Breakdown of compile times for different parts of mathlib"

    h2 "Per-File Timings"
    p "The slowest 10% of files are highlighted in red."
    table $ do
      col ! class_ "file-column"
      col ! class_ "per-file-time-column"
      thead $ tr $ do
        th "File"
        th "Time (s)"
      tbody $ mapM_ renderTiming $ mungePerFileTimings perFileTimings
  where
    commitText = fromCommitHash commitHash
    commitString = T.unpack commitText
    fullBuildTime = diffUTCTime commitEndTime commitStartTime

renderTiming :: PerFileTiming -> Html
renderTiming (PerFileTiming file elapsed highlight) = tr ! highlightAttr $ do
  td $ text file
  td ! class_ "per-file-time-cell" $ string $
    formatTime defaultTimeLocale "%-2Es" elapsed
  where
    highlightAttr :: Attribute
    highlightAttr = if highlight then class_ "per-file-highlight" else mempty
