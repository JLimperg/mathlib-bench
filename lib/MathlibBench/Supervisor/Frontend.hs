{-# LANGUAGE OverloadedStrings #-}

module MathlibBench.Supervisor.Frontend
( frontendMain )
where

import           Control.Monad (void, unless)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import           Data.Time.Clock (getCurrentTime, addUTCTime)
import           Web.Scotty
  ( ActionM, scotty, raw, text, jsonData, get, post, json )
import qualified Web.Scotty as Scotty

import qualified MathlibBench.Api as Api
import           MathlibBench.GitRepo (GitRepoLock)
import qualified MathlibBench.GitRepo as Git
import           MathlibBench.Logging
import           MathlibBench.Secret (Secret)
import           MathlibBench.Supervisor.Config
import           MathlibBench.Supervisor.Db (Connection, ConnectInfo, withConnection)
import qualified MathlibBench.Supervisor.Db as Db
import           MathlibBench.Supervisor.Frontend.Static (globalCss)
import           MathlibBench.Supervisor.Frontend.TimingPage
  ( Timing(..), makeTimingPage )
import           MathlibBench.Supervisor.GitRepo.Timestamp
  ( GitRepoTimestamp, updateGitRepoUnlessUpToDate )
import           MathlibBench.Types

updateCommits :: Connection -> GitRepoLock -> GitRepoTimestamp -> IO ()
updateCommits conn lock timestamp =
  Git.withGitRepoLock lock $ \locked ->
  void $ updateGitRepoUnlessUpToDate locked timestamp $ do
    logInfo "updating git repo"
    Git.pull _WORKDIR locked
    currentHead <- Db.fetchLastCommit conn
    newCommits <- case currentHead of
      Nothing -> Git.getAllCommits _WORKDIR locked
      Just currentHead -> Git.getCommitsFromHeadTo _WORKDIR locked currentHead
    unless (null newCommits) $
      Db.insertCommits conn newCommits
    logInfo "done updating git repo"

setContentTypeHtml :: ActionM ()
setContentTypeHtml = Scotty.setHeader "Content-Type" "text/html; charset=utf-8"

setContentTypeCss :: ActionM ()
setContentTypeCss = Scotty.setHeader "Content-Type" "text/css; charset=utf-8"

frontendMain :: GitRepoLock -> GitRepoTimestamp -> Secret -> ConnectInfo -> IO ()
frontendMain lock timestamp secret connInfo = scotty _PORT $ do
  get "/" $ do
    page <- liftIO $ withConnection connInfo $
      fmap (makeTimingPage . map (uncurry Timing)) . Db.fetchTimings
    setContentTypeHtml
    raw page

  get "/global.css" $ do
    setContentTypeCss
    raw $ BL.fromStrict globalCss

  post "/next" $ do
    Api.validateSecretHeader secret
    next <- liftIO $ withConnection connInfo $ \conn -> do
      updateCommits conn lock timestamp
      currentTime <- getCurrentTime
      Db.pruneTimingsInProgress conn $
        addUTCTime (negate _TIMING_TIMEOUT) currentTime
      nextCommit <- Db.fetchLastUntimedCommit conn
      case nextCommit of
        Nothing -> do
          logInfo "no commit ready for timing"
          pure Api.NoNextCommit
        Just nextCommit -> do
          logInfo $
            "starting timing of commit " <>
            TL.fromStrict (fromCommitHash nextCommit)
          timingInProgressId <-
            Db.insertTimingInProgress conn nextCommit currentTime
          pure $ Api.NextCommit nextCommit timingInProgressId
    json next

  post "/finished" $ do
    Api.validateSecretHeader secret
    (Api.FinishedTiming commit elapsed inProgressId) <- jsonData
    liftIO $ withConnection connInfo $ \conn -> do
      Db.insertTiming conn commit elapsed
      Db.deleteTimingInProgress conn inProgressId
    text ""
