{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
, insertPerFileTimings
, fetchPerFileTimings
)where

import           Control.Exception (bracket)
import           Control.Monad (void)
import           Data.ByteString.Builder (integerDec)
import           Data.Coerce (coerce)
import           Data.Fixed (Fixed(..))
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import           Data.Time
  ( NominalDiffTime, UTCTime, nominalDiffTimeToSeconds
  , secondsToNominalDiffTime )
import           Database.PostgreSQL.Simple
  ( Connection, ConnectInfo, Only(..), execute, execute_, executeMany, query
  , query_ )
import qualified Database.PostgreSQL.Simple as Db
import           Database.PostgreSQL.Simple.FromField (FromField(..))
import           Database.PostgreSQL.Simple.ToField (Action(..), ToField(..))
import           Text.Heredoc (str)

import           MathlibBench.Logging
import           MathlibBench.Types

-- Used to store time durations in the database since NominalDiffTime doesn't
-- have ToField/FromField instances. A NominalDiffTime is an integer
-- representing a time duration in picoseconds, so we just store that integer.
-- Note that this means 1 second is represented as 10^12, so make sure you're
-- using a big enough integer type.
newtype TimeInterval = TimeInterval { _fromTimeInterval :: NominalDiffTime }

instance ToField TimeInterval where
  toField (TimeInterval i) =
    let (MkFixed x) = nominalDiffTimeToSeconds i in
    Plain $ integerDec x

instance FromField TimeInterval where
  fromField field dat
    = TimeInterval . secondsToNominalDiffTime . MkFixed <$> fromField field dat

withConnection :: ConnectInfo -> (Connection -> IO a) -> IO a
withConnection connInfo = bracket (Db.connect connInfo) Db.close

createDb :: Connection -> IO ()
createDb conn = do
  logInfo "setting up database"
  void $ execute_ conn
    [str|create table if not exists commits
        |( id serial primary key
        |, commit_hash text not null unique
        |, commit_time timestamp with time zone not null )
        |]
  void $ execute_ conn
    [str|create table if not exists timings
        |( id serial primary key
        |, commit_id integer not null references commits(id)
        |, start_time timestamp with time zone not null
        |, end_time timestamp with time zone not null
        |, runner text not null )
        |]
  void $ execute_ conn
    [str|create table if not exists inprogress
        |( id serial primary key
        |, commit_id integer not null references commits(id)
        |, start_time timestamp with time zone not null )
        |]
  void $ execute_ conn
    [str|create table if not exists per_file_timings
        |( id serial primary key
        |, commit_id integer not null references commits(id)
        |, file text not null
        |, elapsed bigint not null )
        |]

fetchTimings :: Connection -> IO [(CommitHash, UTCTime, UTCTime, UTCTime, Text)]
fetchTimings conn = query_ conn
  [str|select commits.commit_hash, commits.commit_time, timings.start_time,
      |  timings.end_time, timings.runner
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

insertTiming :: Connection -> CommitHash -> UTCTime -> UTCTime -> Text -> IO ()
insertTiming conn commit startTime endTime runner = void $ execute conn
  [str|insert into timings (commit_id, start_time, end_time, runner)
      |values ((select id from commits where commit_hash = ?), ?, ?, ?)
      |]
  (commit, startTime, endTime, runner)

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

insertCommits :: Connection -> [(CommitHash, UTCTime)] -> IO ()
insertCommits conn commits = void $ executeMany conn
  "insert into commits (commit_hash, commit_time) values (?, ?)"
  commits

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

insertPerFileTimings
  :: Connection -> CommitHash -> [(Text, NominalDiffTime)] -> IO ()
insertPerFileTimings conn commit timings = do
  [Only (commitId :: Int)] <- query conn
    "select id from commits where commit_hash = ?"
    (Only commit)
  void $ executeMany conn
    [str|insert into per_file_timings (commit_id, file, elapsed)
        |values (?, ?, ?)
        |]
    (map (\(file, elapsed) -> (commitId, file, elapsed))
       (coerce timings :: [(Text, TimeInterval)]))

fetchPerFileTimings :: Connection -> CommitHash -> IO [(Text, NominalDiffTime)]
fetchPerFileTimings conn commit = do
  results :: [(Text, TimeInterval)] <- query conn
    [str|select per_file_timings.file, per_file_timings.elapsed
        |from per_file_timings join commits
        |  on per_file_timings.commit_id = commits.id
        |where commits.commit_hash = ?
        |order by per_file_timings.file
        |]
    (Only commit)
  pure $ coerce results
