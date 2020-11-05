{-# LANGUAGE OverloadedStrings #-}

module MathlibBench.Supervisor.Db
( Connection
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

import           Data.Coerce (coerce)
import           Data.Maybe (listToMaybe)
import           Data.String (IsString(fromString))
import qualified Data.Text.Lazy as TL
import           Database.SQLite.Simple
  ( Connection, Only(..), execute, execute_, executeMany, query, query_ )
import qualified Database.SQLite.Simple as Sqlite

import           MathlibBench.Logging
import           MathlibBench.Supervisor.Config (_SQLITE_FILE)
import           MathlibBench.Types
import           MathlibBench.UnixSeconds (UnixSeconds)

withConnection :: (Connection -> IO a) -> IO a
withConnection f = Sqlite.withConnection _SQLITE_FILE $ \conn -> do
  execute_ conn "PRAGMA foreign_keys = ON"
  f conn

createDb :: IO ()
createDb = do
  logInfo $ "setting up database in " <> TL.pack _SQLITE_FILE
  withConnection $ \conn -> do
    execute_ conn $ fromString $ unwords
      [ "CREATE TABLE IF NOT EXISTS commits("
      , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
      , "commit_hash TEXT NOT NULL UNIQUE)"
      ]
    execute_ conn $ fromString $ unwords
      [ "CREATE TABLE IF NOT EXISTS timings ("
      , "id INTEGER PRIMARY KEY,"
      , "commit_id INTEGER NOT NULL,"
      , "elapsed_millis INTEGER NOT NULL,"
      , "FOREIGN KEY(commit_id) REFERENCES commits(id))"
      ]
    execute_ conn $ fromString $ unwords
      [ "CREATE TABLE IF NOT EXISTS inprogress ("
      , "id INTEGER PRIMARY KEY,"
      , "commit_id INTEGER NOT NULL,"
      , "start_time INTEGER NOT NULL,"
      , "FOREIGN KEY(commit_id) REFERENCES commits(id))"
      ]

fetchTimings :: Connection -> IO [(CommitHash, ElapsedTimeMillis)]
fetchTimings conn = query_ conn $ fromString $ unwords
  [ "SELECT commits.commit_hash, timings.elapsed_millis"
  , "FROM timings JOIN commits ON timings.commit_id = commits.id"
  , "ORDER BY commits.id DESC"
  ]

hasTimingForCommit :: Connection -> CommitHash -> IO Bool
hasTimingForCommit conn commit = do
  r <- query conn
    (fromString $ unwords
      [ "SELECT id"
      , "FROM timings JOIN commits ON timings.commit_id = commits.id"
      , "WHERE commits.commit_hash = ?"
      ])
    (Only commit)
    :: IO [Only Int]
  pure $ not $ null r

insertTiming :: Connection -> CommitHash -> ElapsedTimeMillis -> IO ()
insertTiming conn commit time =
    execute conn
      (fromString $ unwords
        [ "INSERT INTO timings (commit_id, elapsed_millis)"
        , "VALUES ((SELECT id FROM commits WHERE commit_hash = ?), ?)"
        ])
      (commit, time)

fetchLastCommit :: Connection -> IO (Maybe CommitHash)
fetchLastCommit conn = do
  r <- query_ conn
    "SELECT commit_hash FROM commits ORDER BY id DESC LIMIT 1"
    :: IO [Only CommitHash]
  pure $ fromOnly <$> listToMaybe r

fetchLastUntimedCommit :: Connection -> IO (Maybe CommitHash)
fetchLastUntimedCommit conn = do
  r <- query conn thequery () :: IO [Only CommitHash]
  pure $ fromOnly <$> listToMaybe r
  where
    thequery = fromString $ unwords
      [ "SELECT commit_hash"
      , "FROM commits"
      , "WHERE NOT EXISTS ("
      , "  SELECT commit_id FROM inprogress"
      , "  WHERE inprogress.commit_id = commits.id)"
      , "AND NOT EXISTS ("
      , "  SELECT commit_id FROM timings"
      , "  WHERE timings.commit_id = commits.id)"
      , "ORDER BY id DESC"
      , "LIMIT 1"
      ]

insertCommits :: Connection -> [CommitHash] -> IO ()
insertCommits conn commits = executeMany conn
  "INSERT INTO commits (commit_hash) VALUES (?)"
  (coerce commits :: [Only CommitHash])

insertTimingInProgress :: Connection -> CommitHash -> UnixSeconds -> IO Int
insertTimingInProgress conn commit startTime = do
  execute conn
    (fromString $ unwords
      [ "INSERT INTO inprogress (commit_id, start_time)"
      , "VALUES ((SELECT id FROM commits WHERE commit_hash = ?), ?)"
      ])
    (commit, startTime)
  fromIntegral <$> Sqlite.lastInsertRowId conn

deleteTimingInProgress :: Connection -> Int -> IO ()
deleteTimingInProgress conn id_ = execute conn
  "DELETE FROM inprogress WHERE id = ?"
  (Only id_)

pruneTimingsInProgress :: Connection -> UnixSeconds -> IO ()
pruneTimingsInProgress conn timeout = execute conn
  "DELETE FROM inprogress WHERE start_time < ?"
  (Only timeout)
