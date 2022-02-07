{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MathlibBench.Supervisor.Db
( Connection
, ConnectInfo(..)
, withConnection
, setupDatabase
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
, fetchTiming
, fetchTimingWithPerFileTimings
) where

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

import           MathlibBench.Types (CommitHash, LinesOfCode)
import           MathlibBench.Supervisor.Db.Schema (setupDatabase)

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

newtype TimingId = TimingId { _fromTimingId :: Int }
  deriving (ToField, FromField)

withConnection :: ConnectInfo -> (Connection -> IO a) -> IO a
withConnection connInfo = bracket (Db.connect connInfo) Db.close

fetchTimings :: Connection -> IO [(CommitHash, UTCTime, UTCTime, UTCTime)]
fetchTimings conn = query_ conn
  [str|select commits.commit_hash, commits.commit_time, timings.start_time, timings.end_time
      |from timings join commits on timings.commit_id = commits.id
      |order by commits.id desc
      |]

hasTimingForCommit :: Connection -> CommitHash -> IO Bool
hasTimingForCommit conn commit = do
  (r :: [Only Int]) <- query conn
    [str|select id
        |from timings join commits on timings.commit_id = commits.id
        |where commits.commit_hash = ?
        |limit 1
        |]
    (Only commit)
  pure $ not $ null r

insertTiming
  :: Connection -> CommitHash -> UTCTime -> UTCTime -> Text -> IO TimingId
insertTiming conn commit startTime endTime runner = do
  [Only timingId] <- query conn
    [str|insert into timings (commit_id, start_time, end_time, runner)
        |values ((select id from commits where commit_hash = ?), ?, ?, ?)
        |returning id
        |]
    (commit, startTime, endTime, runner)
  pure timingId

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
  :: Connection -> TimingId -> [(Text, NominalDiffTime, LinesOfCode)] -> IO ()
insertPerFileTimings conn timingId timings = void $ executeMany conn
  [str|insert into per_file_timings (timing_id, file, elapsed, lines_of_code)
      |values (?, ?, ?, ?)
      |]
  (map (\(file, elapsed, loc) -> (timingId, file, elapsed, loc))
      (coerce timings :: [(Text, TimeInterval, LinesOfCode)]))

-- Returns file name, total compilation time for this file, lines of code (if
-- available) and elapsed time per LOC, i.e. `elapsed / lines_of_code` (if
-- `lines_of_code` is available). Ordered by elapsed time per LOC, highest
-- first.
fetchPerFileTimings
  :: Connection -> TimingId
  -> IO [(Text, NominalDiffTime, Maybe LinesOfCode, Maybe NominalDiffTime)]
fetchPerFileTimings conn timingId = do
  results :: [(Text, TimeInterval, Maybe LinesOfCode, Maybe TimeInterval)] <-
    query conn
      [str|select file, elapsed, lines_of_code,
          |  case
          |    when lines_of_code is null
          |    then null
          |    else elapsed / lines_of_code
          |  end as elapsedPerLoc
          |from per_file_timings
          |where timing_id = ?
          |order by elapsedPerLoc desc nulls last, per_file_timings.file asc
          |]
    (Only timingId)
  pure $ coerce results

fetchTiming
  :: Connection -> CommitHash
  -> IO (Maybe (UTCTime, TimingId, UTCTime, UTCTime, Text))
fetchTiming conn commit = listToMaybe <$>
  query conn
    [str|select commits.commit_time, timings.id, timings.start_time,
        |  timings.end_time, timings.runner
        |from commits join timings on commits.id = timings.commit_id
        |where commits.commit_hash = ?
        |limit 1
        |]
    (Only commit)

fetchTimingWithPerFileTimings
  :: Connection -> CommitHash
  -> IO (Maybe
         (UTCTime, UTCTime, UTCTime, Text,
          [(Text, NominalDiffTime, Maybe LinesOfCode, Maybe NominalDiffTime)]))
fetchTimingWithPerFileTimings conn commit = do
  timingMay <- fetchTiming conn commit
  case timingMay of
    Nothing -> pure Nothing
    Just (commitTime, timingId, startTime, endTime, runner) -> do
     perFileTimings <- fetchPerFileTimings conn timingId
     pure $ Just
       (commitTime, startTime, endTime, runner, perFileTimings)
