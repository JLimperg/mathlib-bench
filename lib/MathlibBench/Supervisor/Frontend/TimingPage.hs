{-# LANGUAGE OverloadedStrings #-}

module MathlibBench.Supervisor.Frontend.TimingPage
( Timing(..)
, renderTimings
) where

import           Prelude hiding (head, id, div)

import           Data.Fixed (Centi)
import           Data.Functor ((<&>))
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
  ( UTCTime, NominalDiffTime, nominalDiffTimeToSeconds, formatTime
  , defaultTimeLocale, diffUTCTime )
import           Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5 as Html
import           Text.Blaze.Html5.Attributes hiding (title)
import qualified Text.Blaze.Html5.Attributes as Attr

import           MathlibBench.Supervisor.Config
import           MathlibBench.Types

data Timing = Timing
  { timingCommit :: CommitHash
  , timingCommitTime :: UTCTime
  , timingRunnerId :: Text
  , timingStartTime :: UTCTime
  , timingEndTime :: UTCTime
  }

timingElapsed :: Timing -> NominalDiffTime
timingElapsed t = diffUTCTime (timingEndTime t) (timingStartTime t)

data TimingRow = TimingRow
  { timingRowCommit :: CommitHash
  , timingRowCommitTime :: UTCTime
  , timingRowRunnerId :: Text
  , timingRowStartTime :: UTCTime
  , timingRowEndTime :: UTCTime
  , timingRowPreviousCommit :: Maybe CommitHash
  , timingRowAbsoluteTimeChange :: Maybe NominalDiffTime
  , timingRowRelativeTimeChange :: Maybe Centi
  }

nominalDiffTimeRelativeChangePercent
  :: NominalDiffTime -> NominalDiffTime -> Centi
nominalDiffTimeRelativeChangePercent t1 t2 = realToFrac $
  ((t2Secs - t1Secs) / t1Secs) * 100
  where
    t1Secs = nominalDiffTimeToSeconds t1
    t2Secs = nominalDiffTimeToSeconds t2

timingsToTimingRows :: [Timing] -> [TimingRow]
timingsToTimingRows [] = []
timingsToTimingRows (t : ts) = row : timingsToTimingRows ts
  where
    prev :: Maybe Timing
    prev = listToMaybe ts

    row = TimingRow
      { timingRowCommit = timingCommit t
      , timingRowCommitTime = timingCommitTime t
      , timingRowRunnerId = timingRunnerId t
      , timingRowStartTime = timingStartTime t
      , timingRowEndTime = timingEndTime t
      , timingRowPreviousCommit = prev <&> timingCommit
      , timingRowAbsoluteTimeChange = prev <&> \prev ->
          timingElapsed t - timingElapsed prev
      , timingRowRelativeTimeChange = prev <&> \prev ->
          nominalDiffTimeRelativeChangePercent (timingElapsed t)
            (timingElapsed prev)
      }

renderDiffLink :: CommitHash -> Maybe CommitHash -> Html
renderDiffLink _ Nothing = ""
renderDiffLink current (Just previous) =
  let url = stringValue $ concat
        [ _DIFF_BASE_URL, "/", T.unpack (fromCommitHash previous), "..."
        , T.unpack (fromCommitHash current) ] in
  mconcat [" (", a "diff" ! href url, ")"]

timingRowClass :: Maybe Centi -> AttributeValue
timingRowClass Nothing = "timing-row-default"
timingRowClass (Just timeRatio)
  | abs timeRatio < _EXPECTED_TIME_DIFF_PERCENT_VARIATION = "timing-row-default"
  | timeRatio > 0 = "timing-row-worse"
  | otherwise = "timing-row-better"

renderTimingRow :: TimingRow -> Html
renderTimingRow
  (TimingRow currentCommit currentCommitTime runnerId startTime endTime previousCommit
   absoluteTimeChange relativeTimeChange)
  = tr ! class_ (timingRowClass relativeTimeChange) $
      mapM_ td
        [ commitCell
        , elapsedTimeCell
        , absoluteTimeChangeCell
        , relativeTimeChangeCell
        ]
  where
    currentCommit' :: Text
    currentCommit' = fromCommitHash currentCommit

    elapsedTime :: NominalDiffTime
    elapsedTime = diffUTCTime endTime startTime

    timestampFormat :: String
    timestampFormat = "%Y-%m-%d %H:%M:%S %Z"

    commitCell :: Html
    commitCell = details $ do
      Html.summary $ do
        a (text $ T.take 8 currentCommit')
          ! href (stringValue $ _COMMIT_BASE_URL ++ "/" ++ T.unpack currentCommit')
          ! Attr.title (textValue currentCommit')
        renderDiffLink currentCommit previousCommit
      Html.span ! class_ "auxiliary-info" $ string $
        "Commited: " <>
          formatTime defaultTimeLocale timestampFormat currentCommitTime

    elapsedTimeCell :: Html
    elapsedTimeCell = details $ do
        Html.summary $ string $
          formatTime defaultTimeLocale "%Hh%Mm%Ss" elapsedTime
        Html.span ! class_ "auxiliary-info" $ do
          string $
            "Started: " <> formatTime defaultTimeLocale timestampFormat startTime
          br
          string $
            "Ended: "   <> formatTime defaultTimeLocale timestampFormat endTime
          br
          a ! href (textValue $ "/perfile/" <> currentCommit') $
            "Per-file timings"
          br
          text $ "Runner: " <> runnerId

    absoluteTimeChangeCell :: Html
    absoluteTimeChangeCell = case absoluteTimeChange of
      Nothing -> "─"
      Just diff -> string $
        if diff >= 0
          then formatTime defaultTimeLocale "%mm%Ss" diff
          else formatTime defaultTimeLocale "-%mm%Ss" (negate diff)

    relativeTimeChangeCell :: Html
    relativeTimeChangeCell = case relativeTimeChange of
      Nothing -> "─"
      Just diff -> string $ show diff


renderTimings' :: [TimingRow] -> Html
renderTimings' timings = docTypeHtml $ do
  head $ do
    meta ! charset "UTF-8"
    link ! rel "stylesheet" ! href "/global.css"
    title "mathlib build time benchmark"

  body $ do
    h1 "mathlib build time benchmark"
    table $ do
      col ! class_ "commit-column"
      col ! class_ "time-column"
      col ! class_ "time-change-absolute-column"
      col ! class_ "time-change-relative-column"
      thead $ tr $ do
        th "commit"
        th "time"
        th "time change"
        th "time change %"
      tbody $ mapM_ renderTimingRow timings

renderTimings :: [Timing] -> Html
renderTimings = renderTimings' . timingsToTimingRows
