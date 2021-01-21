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
import           Text.Blaze.Html5.Attributes hiding (title)
import qualified Text.Blaze.Html5.Attributes as Attr

import           MathlibBench.Supervisor.Config
import           MathlibBench.Types

data Timing = Timing
  { timingCommit :: CommitHash
  , timingStartTime :: UTCTime
  , timingEndTime :: UTCTime
  }

timingElapsed :: Timing -> NominalDiffTime
timingElapsed t = diffUTCTime (timingEndTime t) (timingStartTime t)

data TimingRow = TimingRow
  { timingRowCommit :: CommitHash
  , timingRowStartTime :: UTCTime
  , timingRowEndTime :: UTCTime
  , timingRowPreviousCommit :: Maybe CommitHash
  , timingRowAbsoluteTimeChange :: Maybe NominalDiffTime
  , timingRowRelativeTimeChange :: Maybe Centi
  }

nominalDiffTimeRelativeChangePercent
  :: NominalDiffTime -> NominalDiffTime -> Centi
nominalDiffTimeRelativeChangePercent prev current = realToFrac $
  (currentSecs / prevSecs - 1) * 100
  where
    prevSecs = nominalDiffTimeToSeconds prev
    currentSecs = nominalDiffTimeToSeconds current

timingsToTimingRows :: [Timing] -> [TimingRow]
timingsToTimingRows [] = []
timingsToTimingRows (t : ts) = row : timingsToTimingRows ts
  where
    prev :: Maybe Timing
    prev = listToMaybe ts

    row = TimingRow
      { timingRowCommit = timingCommit t
      , timingRowStartTime = timingStartTime t
      , timingRowEndTime = timingEndTime t
      , timingRowPreviousCommit = prev <&> timingCommit
      , timingRowAbsoluteTimeChange = prev <&> \prev ->
          timingElapsed t - timingElapsed prev
      , timingRowRelativeTimeChange = prev <&> \prev ->
          nominalDiffTimeRelativeChangePercent (timingElapsed prev)
            (timingElapsed t)
      }

renderDiffLink :: CommitHash -> Maybe CommitHash -> Html
renderDiffLink _ Nothing = mempty
renderDiffLink current (Just previous) =
  let url = stringValue $ concat
        [ _DIFF_BASE_URL, "/", T.unpack (fromCommitHash previous), "..."
        , T.unpack (fromCommitHash current) ] in
  mconcat
    [ " ("
    , a "diff"
        ! href url
        ! Attr.title "Diff between this commit and the last benchmarked commit"
    , ")"]

timingRowClass :: Maybe Centi -> AttributeValue
timingRowClass Nothing = "timing-row-default"
timingRowClass (Just timeRatio)
  | abs timeRatio < _EXPECTED_TIME_DIFF_PERCENT_VARIATION = "timing-row-default"
  | timeRatio > 0 = "timing-row-worse"
  | otherwise = "timing-row-better"

renderTimingRow :: TimingRow -> Html
renderTimingRow
  (TimingRow currentCommit startTime endTime previousCommit
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

    commitCell :: Html
    commitCell = do
      a (text $ T.take 8 currentCommit')
        ! href (textValue $ "/commit/" <> currentCommit')
        ! Attr.title (textValue currentCommit')
      renderDiffLink currentCommit previousCommit

    elapsedTimeCell :: Html
    elapsedTimeCell = string $
      formatTime defaultTimeLocale "%Hh%Mm%Ss" elapsedTime

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
    title "mathlib Build Time Benchmark"

  body $ do
    h1 "mathlib Build Time Benchmark"
    table ! id "timings-table" $ do
      col ! class_ "commit-column"
      col ! class_ "time-column"
      col ! class_ "time-change-absolute-column"
      col ! class_ "time-change-relative-column"
      thead $ tr $ do
        th "Commit"
        th "Time"
        th "Time change"
        th "Time change %"
      tbody $ mapM_ renderTimingRow timings

renderTimings :: [Timing] -> Html
renderTimings = renderTimings' . timingsToTimingRows
