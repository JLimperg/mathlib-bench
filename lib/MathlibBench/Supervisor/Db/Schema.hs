{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module MathlibBench.Supervisor.Db.Schema (setupDatabase) where

import           Control.Monad (void, forM_)
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import           Database.PostgreSQL.Simple
  ( Connection, Only(..), execute, execute_, query, query_ )
import           Database.PostgreSQL.Simple.FromField (FromField(..))
import           Database.PostgreSQL.Simple.ToField (ToField(..))
import qualified Database.PostgreSQL.Simple as Db
import           Text.Heredoc (str)

import           MathlibBench.Logging (logInfo)

newtype SchemaVersion = SchemaVersion { fromSchemaVersion :: Int }
  deriving (FromField, ToField)

createInitialSchema :: Connection -> IO ()
createInitialSchema conn = do
  logInfo "creating initial schema"
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
        |, timing_id integer not null references timings(id)
        |, file text not null
        |, elapsed bigint not null )
        |]
  void $ execute_ conn
    [str|create table if not exists schema_version
        |( version integer not null )
        |]

fetchSchemaVersion :: Connection -> IO SchemaVersion
fetchSchemaVersion conn = do
  result <- query_ conn [str|select (version) from schema_version limit 1|]
  case result of
    [] -> pure $ SchemaVersion 0
    [Only version] -> pure version
    _ -> error "unreachable"

data Migration = Migration
  { migrationName :: Text
  , migrationAction :: Connection -> IO ()
  }

migration1 :: Migration
migration1 = Migration
  { migrationName = "2022-02-06-add-lines_of_code"
  , migrationAction = \conn ->
      void $ execute_ conn
        [str|alter table per_file_timings
            |add column lines_of_code integer
            |]
  }

migrations :: [Migration]
migrations = [migration1]

targetSchemaVersion :: Int
targetSchemaVersion = length migrations

migrationsToApply :: SchemaVersion -> [Migration]
migrationsToApply (SchemaVersion currentSchemaVersion) =
  drop currentSchemaVersion migrations

setupDatabase :: Connection -> IO ()
setupDatabase conn = do
  logInfo "setting up the database"
  -- Create the initial schema if it doesn't exist.
  createInitialSchema conn

  -- Apply migrations
  currentSchemaVersion <- fetchSchemaVersion conn
  logInfo $ "current schema version: " <>
    TL.pack (show $ fromSchemaVersion currentSchemaVersion)
  forM_ (migrationsToApply currentSchemaVersion) $ \migration -> do
    logInfo $ TL.fromStrict $ "running migration " <> migrationName migration
    migrationAction migration conn

  -- Update the schema version
  logInfo $ "new schema version: " <> TL.pack (show targetSchemaVersion)

  void $ execute_ conn
    [str|delete from schema_version|]

  void $ execute conn
    [str|insert into schema_version (version) values (?)|]
    (Only $ SchemaVersion $ length migrations)
