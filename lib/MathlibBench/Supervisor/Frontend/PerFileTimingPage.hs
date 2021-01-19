{-# LANGUAGE OverloadedStrings #-}

module MathlibBench.Supervisor.Frontend.PerFileTimingPage
( renderPerFileTimings )
where

import           Prelude hiding (head)

import           Data.Text (Text)
import           Data.Time (NominalDiffTime, formatTime, defaultTimeLocale)
import           Text.Blaze.Html5 hiding (map)
import           Text.Blaze.Html5.Attributes hiding (title)

import           MathlibBench.Types

renderPerFileTimings :: CommitHash -> [(Text, NominalDiffTime)] -> Html
renderPerFileTimings commit timings = docTypeHtml $ do
  head $ do
    meta ! charset "UTF-8"
    link ! rel "stylesheet" ! href "global.css"
    title $ text $ "per-file timings for commit " <> commitText

  body $ do
    h1 $ text $ "per-file timings for commit " <> commitText
    table $ do
      col ! class_ "file-column"
      col ! class_ "elapsed-column"
      thead $ tr $ do
        th "file"
        th "time"
      tbody $ mapM_ (uncurry renderTiming) timings
  where
    commitText = fromCommitHash commit

renderTiming :: Text -> NominalDiffTime -> Html
renderTiming file elapsed = tr $ do
  td $ text file
  td $ string $ formatTime defaultTimeLocale "%Hh%Mm%Ss" elapsed
