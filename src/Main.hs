{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Config
import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.SQLite.Simple
  ( query, Only(..), execute, ToRow(..), FromRow(..), withConnection, execute_
  , field )
import Database.SQLite.Simple.FromField (FromField(..))
import Database.SQLite.Simple.ToField (ToField(..))
import System.Directory
  ( doesDirectoryExist, withCurrentDirectory, createDirectoryIfMissing )
import System.Process.Typed (readProcess_, runProcess_, proc)

newtype CommitHash = CommitHash { fromCommitHash :: Text }

instance FromField CommitHash where
  fromField = fmap CommitHash . fromField

instance ToField CommitHash where
  toField = toField . fromCommitHash

newtype ElapsedTimeMillis = ElapsedTimeMillis { fromElapsedTimeMillis :: Int }

instance FromField ElapsedTimeMillis where
  fromField = fmap ElapsedTimeMillis . fromField

instance ToField ElapsedTimeMillis where
  toField = toField . fromElapsedTimeMillis

nominalDiffTimeToElapsedTimeMillis :: NominalDiffTime -> ElapsedTimeMillis
nominalDiffTimeToElapsedTimeMillis = ElapsedTimeMillis . floor . (1e3 *)

elapsedTimeMillisToNominalDiffTime :: ElapsedTimeMillis -> NominalDiffTime
elapsedTimeMillisToNominalDiffTime = (/ 1e3) . fromIntegral . fromElapsedTimeMillis


data Timing = Timing
  { timingId :: Int
  , timingCommit :: Text
  , timingElapsed :: ElapsedTimeMillis
  }

instance FromRow Timing where
  fromRow = Timing <$> field <*> field <*> field

instance ToRow Timing where
  toRow (Timing id_ commit elapsed) = toRow (id_, commit, elapsed)

setupRootDir :: IO ()
setupRootDir = createDirectoryIfMissing True _ROOTDIR

setupGitRepo :: IO ()
setupGitRepo = do
  workDirExists <- doesDirectoryExist _WORKDIR
  unless workDirExists $
    runProcess_ $ proc "git" ["clone", _MATHLIB_GIT_URL, _WORKDIR]

setupDb :: IO ()
setupDb = withConnection _SQLITE_FILE $ \conn -> do
  execute_ conn "CREATE TABLE IF NOT EXISTS timings (id INTEGER PRIMARY KEY, commit_hash TEXT, elapsed_millis INTEGER)"

getHeadCommit :: IO CommitHash
getHeadCommit = withCurrentDirectory _WORKDIR $ do
  runProcess_ $ proc "git" ["pull"]
  (stdout, _) <- readProcess_ (proc "git" ["log", "-n1", "--format=format:%H"])
  pure $ CommitHash $ T.decodeUtf8 $ BL.toStrict stdout

hasTimingForCommit :: CommitHash -> IO Bool
hasTimingForCommit commit = do
  withConnection _SQLITE_FILE $ \conn -> do
    r <- query conn "SELECT id FROM timings WHERE commit_hash = ?" (Only commit) :: IO [Only Int]
    pure $ not $ null r

timeBuild :: CommitHash -> IO ElapsedTimeMillis
timeBuild commit = withCurrentDirectory _WORKDIR $ do
  runProcess_ $ proc "git" ["reset", "--hard", "HEAD"]
  runProcess_ $ proc "git" ["clean", "-fdx"]
  runProcess_ $ proc  _LEANPKG ["configure"]
  startTime <- getPOSIXTime
  runProcess_ $ proc _LEANPKG
    [ "build", "--", "--threads", show _NUM_THREADS, "--memory"
    , show _MEM_LIMIT_MB ]
  endTime <- getPOSIXTime
  pure $ nominalDiffTimeToElapsedTimeMillis $ endTime - startTime

writeTiming :: CommitHash -> ElapsedTimeMillis -> IO ()
writeTiming commit time = withConnection _SQLITE_FILE $ \conn ->
  execute conn "INSERT INTO timings (commit_hash, elapsed_millis) VALUES (?, ?)" (commit, time)

main :: IO ()
main = do
  setupRootDir
  setupDb
  setupGitRepo
  forever $ do
    headCommit <- getHeadCommit
    hasTiming <- hasTimingForCommit headCommit
    unless hasTiming $ do
      elapsed <- timeBuild headCommit
      writeTiming headCommit elapsed
    threadDelay $ 5*60*1000000
