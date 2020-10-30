{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Prelude hiding (head, id, div)

import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BL
import           Data.Fixed (Centi)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
  (NominalDiffTime, nominalDiffTimeToSeconds, formatTime, defaultTimeLocale)
import           Database.SQLite.Simple (Connection, query_, withConnection)
import           Text.Blaze.Html4.Strict hiding (map)
import           Text.Blaze.Html4.Strict.Attributes hiding (title)
import qualified Text.Blaze.Html4.Strict.Attributes as Attr
import qualified Text.Blaze.Renderer.Utf8 as BlazeUtf8
import           Web.Scotty (ActionM, scotty, get)
import qualified Web.Scotty as Scotty

import MathlibBench.Config
import MathlibBench.Frontend.Config
import MathlibBench.Frontend.Static
import MathlibBench.Types

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

loadTimings :: Connection -> IO [Timing]
loadTimings conn = query_ conn "SELECT * FROM timings ORDER BY id DESC"

formatElapsedTime :: ElapsedTimeMillis -> Html
formatElapsedTime
  = string
  . formatTime defaultTimeLocale "%Hh%Mm%Ss"
  . elapsedTimeMillisToNominalDiffTime

renderDiffLink :: CommitHash -> Maybe CommitHash -> Html
renderDiffLink _ Nothing = ""
renderDiffLink current (Just previous) =
  let url = stringValue $ concat
        [ _DIFF_BASE_URL, "/", T.unpack (fromCommitHash previous), ".."
        , T.unpack (fromCommitHash current) ] in
  mconcat [" (", a "diff" ! href url, ")"]

timingRowClass :: DisplayTiming -> AttributeValue
timingRowClass (DisplayTiming _ Nothing) = "timing-row-default"
timingRowClass (DisplayTiming _ (Just previous)) =
  if previousTimingTimeDiff previous > 0
    then "timing-row-worse"
    else "timing-row-better"

renderDisplayTiming :: DisplayTiming -> Html
renderDisplayTiming timing@(DisplayTiming current previous) = tr $
  forM_ [commit, elapsedTime, timeChangeAbsolute, timeChangeRelative] $ \row ->
    td row ! class_ (timingRowClass timing)
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
        string $ formatTime defaultTimeLocale "%mm%Ss" $
          previousTimingTimeDiff previous

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

makeTimingPage :: Connection -> IO BL.ByteString
makeTimingPage conn = do
  timings <- loadTimings conn
  pure $ BlazeUtf8.renderMarkup $ renderTimings $ timingsToDisplayTimings timings

setContentTypeHtml :: ActionM ()
setContentTypeHtml = Scotty.setHeader "Content-Type" "text/html; charset=utf-8"

setContentTypeCss :: ActionM ()
setContentTypeCss = Scotty.setHeader "Content-Type" "text/css; charset=utf-8"

main :: IO ()
main = scotty 8080 $ do
  get "/" $ do
    page <- liftIO $ withConnection _SQLITE_FILE makeTimingPage
    setContentTypeHtml
    Scotty.raw page

  get "/global.css" $ do
    setContentTypeCss
    Scotty.raw $ BL.fromStrict globalCss
