{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module MathlibBench.Supervisor.Frontend.TimingPage
( Timing(..)
, makeTimingPage
) where

import           Prelude hiding (head, id, div)

import qualified Data.ByteString.Lazy as BL
import           Data.Fixed (Centi)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
  (NominalDiffTime, nominalDiffTimeToSeconds, formatTime, defaultTimeLocale)
import           Text.Blaze.Html4.Strict hiding (map)
import           Text.Blaze.Html4.Strict.Attributes hiding (title)
import qualified Text.Blaze.Html4.Strict.Attributes as Attr
import qualified Text.Blaze.Renderer.Utf8 as BlazeUtf8

import MathlibBench.Supervisor.Config
import MathlibBench.Types

data Timing = Timing
  { timingCommit :: CommitHash
  , timingElapsed :: ElapsedTimeMillis
  }

data PreviousTiming = PreviousTiming
  { previousTimingCommit :: CommitHash
  , previousTimingTimeDiff :: NominalDiffTime
  , previousTimingTimeRatio :: Centi
  }

data DisplayTiming = DisplayTiming
  { _displayTimingTiming :: Timing
  , _displayTimingPrevious :: Maybe PreviousTiming
  }

makePreviousTiming :: Timing -> Timing -> PreviousTiming
makePreviousTiming current previous = PreviousTiming
  { previousTimingCommit = timingCommit previous
  , previousTimingTimeDiff
      = elapsedCurrent - elapsedPrevious
  , previousTimingTimeRatio
      = realToFrac $
          (nominalDiffTimeToSeconds elapsedCurrent / nominalDiffTimeToSeconds elapsedPrevious - 1) * 100
  }
  where
    elapsedCurrent = elapsedTimeMillisToNominalDiffTime $ timingElapsed current
    elapsedPrevious = elapsedTimeMillisToNominalDiffTime $ timingElapsed previous

makeDisplayTiming :: Timing -> Timing -> DisplayTiming
makeDisplayTiming current previous
  = DisplayTiming current $ Just $ makePreviousTiming current previous

timingsToDisplayTimings :: [Timing] -> [DisplayTiming]
timingsToDisplayTimings [] = []
timingsToDisplayTimings timings
  = zipWith makeDisplayTiming timings (tail timings) ++
      [DisplayTiming (last timings) Nothing]

formatElapsedTime :: ElapsedTimeMillis -> Html
formatElapsedTime
  = string
  . formatTime defaultTimeLocale "%Hh%Mm%Ss"
  . elapsedTimeMillisToNominalDiffTime

renderDiffLink :: CommitHash -> Maybe CommitHash -> Html
renderDiffLink _ Nothing = ""
renderDiffLink current (Just previous) =
  let url = stringValue $ concat
        [ _DIFF_BASE_URL, "/", T.unpack (fromCommitHash previous), "..."
        , T.unpack (fromCommitHash current) ] in
  mconcat [" (", a "diff" ! href url, ")"]

timingRowClass :: DisplayTiming -> AttributeValue
timingRowClass (DisplayTiming _ Nothing) = "timing-row-default"
timingRowClass (DisplayTiming _ (Just previous))
  | abs ratio < _EXPECTED_TIME_DIFF_PERCENT_VARIATION = "timing-row-default"
  | ratio > 0 = "timing-row-worse"
  | otherwise = "timing-row-better"
  where
    ratio = previousTimingTimeRatio previous

renderDisplayTiming :: DisplayTiming -> Html
renderDisplayTiming timing@(DisplayTiming current previous)
  = tr ! class_ (timingRowClass timing) $
      mapM_ td [commit, elapsedTime, timeChangeAbsolute, timeChangeRelative]
  where
    currentCommit :: Text
    currentCommit = fromCommitHash $ timingCommit current

    commit :: Html
    commit = mconcat
      [ a (text $ T.take 8 $ currentCommit)
        ! href (stringValue $ _COMMIT_BASE_URL ++ "/" ++ T.unpack currentCommit)
        ! Attr.title (textValue currentCommit)
      , renderDiffLink (timingCommit current) (previousTimingCommit <$> previous)
      ]

    elapsedTime :: Html
    elapsedTime = formatElapsedTime $ timingElapsed current

    timeChangeAbsolute :: Html
    timeChangeAbsolute = case previous of
      Nothing -> "─"
      Just previous ->
        let diff = previousTimingTimeDiff previous in
        let isPositive = diff >= 0 in
        string $
          if isPositive
            then formatTime defaultTimeLocale "%mm%Ss" diff
            else formatTime defaultTimeLocale "-%mm%Ss" (negate diff)

    timeChangeRelative :: Html
    timeChangeRelative = case previous of
      Nothing -> "─"
      Just previous -> string $ show $ previousTimingTimeRatio previous


renderTimings :: [DisplayTiming] -> Html
renderTimings timings = docTypeHtml $ do
  head $ do
    meta ! charset "UTF-8"
    link ! rel "stylesheet" ! href "/global.css"
    title "mathlib build time benchmark"

  body $ do
    h1 "mathlib build time benchmark"
    table $ do
      tr $ do
        th "commit"
        th "time"
        th "time change"
        th "time change %"
      mconcat $ map renderDisplayTiming timings

makeTimingPage :: [Timing] -> BL.ByteString
makeTimingPage = BlazeUtf8.renderMarkup . renderTimings . timingsToDisplayTimings
