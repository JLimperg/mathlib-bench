{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module MathlibBench.Supervisor.Db
( Connection
, ConnectInfo(..)
, withConnection
, createDb
, fetchTimings
, hasTimingForCommit
, insertTiming
, fetchLastCommit
, fetchLastUntimedCommit
, insertCommits
, insertTimingInProgress
, deleteTimingInProgress
, pruneTimingsInProgress
)where

import           Control.Exception (bracket)
import           Control.Monad (void)
import           Data.Coerce (coerce)
import           Data.Maybe (listToMaybe)
import           Data.String (IsString(fromString))
import           Data.Time (UTCTime)
import           Database.PostgreSQL.Simple
  ( Connection, ConnectInfo, Only(..), execute, execute_, executeMany, query, query_ )
import qualified Database.PostgreSQL.Simple as Db
import           Text.Heredoc (str)

import           MathlibBench.Logging
import           MathlibBench.Types

withConnection :: ConnectInfo -> (Connection -> IO a) -> IO a
withConnection connInfo = bracket (Db.connect connInfo) Db.close

createDb :: Connection -> IO ()
createDb conn = do
  logInfo "setting up database"
  void $ execute_ conn
    [str|create table if not exists commits
        |( id serial primary key
        |, commit_hash text not null unique )
        |]
  void $ execute_ conn
    [str|create table if not exists timings
        |( id serial primary key
        |, commit_id integer not null references commits(id)
        |, elapsed_millis integer not null )
        |]
  void $ execute_ conn
    [str|create table if not exists inprogress
        |( id serial primary key
        |, commit_id integer not null references commits(id)
        |, start_time timestamp with time zone not null )
        |]

fetchTimings :: Connection -> IO [(CommitHash, ElapsedTimeMillis)]
fetchTimings conn = query_ conn
  [str|select commits.commit_hash, timings.elapsed_millis
      |from timings join commits on timings.commit_id = commits.id
      |order by commits.id desc
      |]

hasTimingForCommit :: Connection -> CommitHash -> IO Bool
hasTimingForCommit conn commit = do
  r <- query conn
    [str|select id
        |from timings join commits on timings.commit_id = commits.id
        |where commits.commit_hash = ?
        |]
    (Only commit)
    :: IO [Only Int]
  pure $ not $ null r

insertTiming :: Connection -> CommitHash -> ElapsedTimeMillis -> IO ()
insertTiming conn commit time = void $ execute conn
  [str|insert into timings (commit_id, elapsed_millis)
      |values ((select id from commits where commit_hash = ?), ?)
      |]
  (commit, time)

fetchLastCommit :: Connection -> IO (Maybe CommitHash)
fetchLastCommit conn = do
  r <- query_ conn
    "select commit_hash from commits order by id desc limit 1"
    :: IO [Only CommitHash]
  pure $ fromOnly <$> listToMaybe r

fetchLastUntimedCommit :: Connection -> IO (Maybe CommitHash)
fetchLastUntimedCommit conn = do
  r <- query_ conn
    [str|select commit_hash from commits
        |where not exists (
        |  select commit_id from inprogress
        |  where inprogress.commit_id = commits.id )
        |and not exists (
        |  select commit_id from timings
        |  where timings.commit_id = commits.id )
        |order by id desc
        |limit 1
        |]
    :: IO [Only CommitHash]
  pure $ fromOnly <$> listToMaybe r

insertCommits :: Connection -> [CommitHash] -> IO ()
insertCommits conn commits = void $ executeMany conn
  "insert into commits (commit_hash) values (?)"
  (coerce commits :: [Only CommitHash])

insertTimingInProgress :: Connection -> CommitHash -> UTCTime -> IO Int
insertTimingInProgress conn commit startTime = do
  [Only rid] <- query conn
    [str|insert into inprogress (commit_id, start_time)
        |values ((select id from commits where commit_hash = ?), ?)
        |returning id
        |]
    (commit, startTime)
  pure rid

deleteTimingInProgress :: Connection -> Int -> IO ()
deleteTimingInProgress conn id_ = void $ execute conn
  "delete from inprogress where id = ?"
  (Only id_)

pruneTimingsInProgress :: Connection -> UTCTime -> IO ()
pruneTimingsInProgress conn timeout = void $ execute conn
  "delete from inprogress where start_time < ?"
  (Only timeout)
