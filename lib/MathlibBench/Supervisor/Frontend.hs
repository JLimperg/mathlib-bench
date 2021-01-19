{-# LANGUAGE OverloadedStrings #-}

module MathlibBench.Supervisor.Frontend
( frontendMain )
where

import           Control.Monad (void, unless)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import           Data.Time.Clock (getCurrentTime, addUTCTime)
import qualified Text.Blaze.Renderer.Utf8 as BlazeUtf8
import           Text.Blaze.XHtml5 (Html)
import           Web.Scotty
  ( ActionM, scotty, raw, text, jsonData, get, post, json, param )
import qualified Web.Scotty as Scotty

import qualified MathlibBench.Api as Api
import           MathlibBench.GitRepo (GitRepoLock)
import qualified MathlibBench.GitRepo as Git
import           MathlibBench.Logging
import           MathlibBench.Secret (Secret)
import           MathlibBench.Supervisor.Config
import           MathlibBench.Supervisor.Db
  ( Connection, ConnectInfo, withConnection )
import qualified MathlibBench.Supervisor.Db as Db
import           MathlibBench.Supervisor.Frontend.Static (globalCss)
import           MathlibBench.Supervisor.Frontend.PerFileTimingPage
  ( renderPerFileTimings )
import           MathlibBench.Supervisor.Frontend.TimingPage
  ( Timing(..), renderTimings )
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
    unless (null newCommits) $ Db.insertCommits conn newCommits
    logInfo "done updating git repo"

setContentTypeHtml :: ActionM ()
setContentTypeHtml = Scotty.setHeader "Content-Type" "text/html; charset=utf-8"

setContentTypeCss :: ActionM ()
setContentTypeCss = Scotty.setHeader "Content-Type" "text/css; charset=utf-8"

blaze :: Html -> ActionM ()
blaze page = do
  setContentTypeHtml
  raw $ BlazeUtf8.renderMarkup page

frontendMain ::
  GitRepoLock -> GitRepoTimestamp -> Secret -> ConnectInfo -> Int -> IO ()
frontendMain lock timestamp secret connInfo port = scotty port $ do
  get "/" $ do
    page <- liftIO $ withConnection connInfo $ \conn -> do
      timings <- Db.fetchTimings conn
      pure $ renderTimings $
        map
          (\(commit, commitTime, startTime, endTime, runner) ->
             Timing commit commitTime runner startTime endTime)
          timings
    blaze page

  get "/global.css" $ do
    setContentTypeCss
    raw $ BL.fromStrict globalCss

  get "/perfile/:commit" $ do
    commit' <- param "commit"
    let commit = CommitHash commit'
    page <- liftIO $ withConnection connInfo $ \conn -> do
      perFileTimings <- Db.fetchPerFileTimings conn commit
      pure $ renderPerFileTimings commit perFileTimings
    blaze page

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
    (Api.FinishedTiming commit inProgressId startTime endTime runnerId perFileTimings) <-
      jsonData
    liftIO $ logInfo $ mconcat
       [ "runner \"", TL.fromStrict runnerId, "\" reports timing for commit "
       , TL.fromStrict (fromCommitHash commit) ]
    liftIO $ withConnection connInfo $ \conn -> do
      Db.insertTiming conn commit startTime endTime runnerId
      Db.insertPerFileTimings conn commit (Map.toList perFileTimings)
      Db.deleteTimingInProgress conn inProgressId
    text ""
