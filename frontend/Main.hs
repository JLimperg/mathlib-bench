{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import           Prelude hiding (head, id, div)

import qualified Data.ByteString.Lazy as BL
import           Database.SQLite.Simple (Connection, query_, withConnection)
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import           Text.Blaze.Html4.Strict hiding (map)
import qualified Text.Blaze.Renderer.Utf8 as BlazeUtf8

import MathlibBench.Config
import MathlibBench.Types

loadTimings :: Connection -> IO [Timing]
loadTimings conn = query_ conn "SELECT * FROM timings ORDER BY id ASC"

renderTiming :: Timing -> Html
renderTiming Timing { timingCommit = commit, timingElapsed = elapsed } = tr $ do
  td $ text commit
  td $ string $ show $ fromElapsedTimeMillis elapsed

renderTimings :: [Timing] -> Html
renderTimings timings = docTypeHtml $ do
  head $
    title "mathlib per-commit build times"

  body $ do
    h1 "mathlib per-commit build times"
    table $ do
      tr $ do
        th "commit"
        th "elapsed time (milliseconds)"
      mconcat $ map renderTiming timings

makeTimingPage :: Connection -> IO BL.ByteString
makeTimingPage conn = do
  timings <- loadTimings conn
  pure $ BlazeUtf8.renderMarkup $ renderTimings timings

app :: Wai.Application
app _ respond = do
  page <- withConnection _SQLITE_FILE makeTimingPage
  respond $ Wai.responseLBS
      HTTP.status200
      [("Content-Type", "text/html; charset=uft-8")]
      page

main :: IO ()
main = Warp.run 8080 app
