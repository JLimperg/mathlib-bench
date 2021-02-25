module MathlibBench.Runner.CmdArgs
( DaemonCmdArgs(..)
, OneShotCmdArgs(..)
, CmdArgs(..)
, parseCmdArgs
) where

import           Data.String (fromString)
import           Data.Text (Text)
import           Options.Applicative
import           Numeric.Natural (Natural)

import           MathlibBench.Secret (Secret, parseSecret)
import           MathlibBench.Types (CommitHash(..))

data DaemonCmdArgs = DaemonCmdArgs
  { daemonCmdArgsRunnerId :: Text
  , daemonCmdArgsSupervisorUrl :: String
  , daemonCmdArgsSecret :: Secret
  }

daemonOptsParser :: Parser DaemonCmdArgs
daemonOptsParser = DaemonCmdArgs
  <$> argument str
        (  metavar "RUNNER_ID"
        <> help "A unique name for this runner." )
  <*> argument str
        (  metavar "SUPERVISOR_URL"
        <> help "Base URL of the supervisor." )
  <*> argument (maybeReader $ parseSecret . fromString)
        (  metavar "SECRET"
        <> help "A secret token shared with the supervisor." )

daemonProgramInfo :: ParserInfo DaemonCmdArgs
daemonProgramInfo = info (daemonOptsParser <**> helper)
  $  progDesc "Run a daemon which continuously benchmarks commits."

data OneShotCmdArgs = OneShotCmdArgs
  { oneShotCmdArgsCommit :: CommitHash
  , oneShotCmdArgsRuns :: Natural
  }

oneShotOptsParser :: Parser OneShotCmdArgs
oneShotOptsParser = OneShotCmdArgs
  <$> argument (CommitHash <$> str)
        (  metavar "COMMIT"
        <> help "Full hash of the commit to be benchmarked." )
  <*> option auto
        (  long "runs"
        <> metavar "N"
        <> value 1
        <> help "How many times the commit should be benchmarked." )

oneShotProgramInfo :: ParserInfo OneShotCmdArgs
oneShotProgramInfo = info (oneShotOptsParser <**> helper)
  $  progDesc "Run a one-off benchmark for a specific commit."

data CmdArgs
  = Daemon DaemonCmdArgs
  | OneShot OneShotCmdArgs

optsParser :: Parser CmdArgs
optsParser = subparser
  $  command "daemon" (Daemon <$> daemonProgramInfo)
  <> command "oneshot" (OneShot <$> oneShotProgramInfo)

programInfo :: ParserInfo CmdArgs
programInfo = info (optsParser <**> helper)
  $  progDesc "mathlib-bench benchmark runner"

parseCmdArgs :: IO CmdArgs
parseCmdArgs = execParser programInfo
