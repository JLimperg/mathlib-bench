{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Prelude hiding (head, id, div)

import qualified Data.ByteString.Lazy as BL
import           Data.Fixed (Centi)
import qualified Data.Text as T
import           Data.Time
  (NominalDiffTime, nominalDiffTimeToSeconds, formatTime, defaultTimeLocale)
import           Database.SQLite.Simple (Connection, query_, withConnection)
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import           Text.Blaze.Html4.Strict hiding (map)
import           Text.Blaze.Html4.Strict.Attributes hiding (title)
import qualified Text.Blaze.Html4.Strict.Attributes as Attr
import qualified Text.Blaze.Renderer.Utf8 as BlazeUtf8

import MathlibBench.Config
import MathlibBench.Frontend.Config
import MathlibBench.Types

data PreviousTiming = PreviousTiming
  { previousTimingCommit :: CommitHash
  , previousTimingTimeDiff :: NominalDiffTime
  , previousTimingTimeRatio :: Centi
  }

data DisplayTiming = DisplayTiming
  { displayTimingTiming :: Timing
  , displayTimingPrevious :: Maybe PreviousTiming
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

loadTimings :: Connection -> IO [Timing]
loadTimings conn = query_ conn "SELECT * FROM timings ORDER BY id DESC"

formatElapsedTime :: ElapsedTimeMillis -> Html
formatElapsedTime
  = string
  . formatTime defaultTimeLocale "%Hh%Mm%Ss"
  . elapsedTimeMillisToNominalDiffTime

renderMaybe :: Maybe a -> (a -> Html) -> Html
renderMaybe ma f = maybe "─" f ma

renderDiffLink :: CommitHash -> CommitHash -> Html
renderDiffLink current previous =
  let url = stringValue $ concat
        [ _DIFF_BASE_URL, "/", T.unpack (fromCommitHash previous), ".."
        , T.unpack (fromCommitHash current) ] in
  a "diff" ! href url

renderDisplayTiming :: DisplayTiming -> Html
renderDisplayTiming (DisplayTiming current previous) = tr $ do
  td $
    a (text $ T.take 8 $ currentCommit)
    ! href (stringValue $ _COMMIT_BASE_URL ++ "/" ++ T.unpack currentCommit)
    ! Attr.title (textValue currentCommit)
  td $ formatElapsedTime $ timingElapsed current
  td $ renderMaybe previous $ string . formatTime defaultTimeLocale "%mm%Ss" . previousTimingTimeDiff
  td $ renderMaybe previous $ string . show . previousTimingTimeRatio
  td $ renderMaybe previous $ \previous ->
    renderDiffLink (timingCommit current) (previousTimingCommit previous)
  where
    currentCommit = fromCommitHash $ timingCommit current

renderTimings :: [DisplayTiming] -> Html
renderTimings timings = docTypeHtml $ do
  head $ do
    meta ! charset "UTF-8"
    title "mathlib build time benchmark"

  body $ do
    h1 "mathlib build time benchmark"
    table $ do
      tr $ do
        th "commit"
        th "elapsed time"
        th "time diff"
        th "%"
        th "diff"
      mconcat $ map renderDisplayTiming timings

makeTimingPage :: Connection -> IO BL.ByteString
makeTimingPage conn = do
  timings <- loadTimings conn
  pure $ BlazeUtf8.renderMarkup $ renderTimings $ timingsToDisplayTimings timings

app :: Wai.Application
app _ respond = do
  page <- withConnection _SQLITE_FILE makeTimingPage
  respond $ Wai.responseLBS
      HTTP.status200
      [("Content-Type", "text/html; charset=uft-8")]
      page

main :: IO ()
main = Warp.run 8080 app
