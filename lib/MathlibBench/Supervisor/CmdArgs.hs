{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module MathlibBench.Supervisor.CmdArgs
( CmdArgs(..)
, parseCmdArgs
) where

import           Data.String (fromString)
import           Options.Applicative

import           MathlibBench.Secret (Secret, parseSecret)
import           MathlibBench.Supervisor.Config (_DEFAULT_PORT)
import qualified MathlibBench.Supervisor.Zulip as Zulip

data CmdArgs = CmdArgs
  { cmdArgsSecret :: Secret
  , cmdArgsDatabasePassword :: String
  , cmdArgsPort :: Int
  , cmdArgsZulipData :: Maybe Zulip.MessageMetadata
  }

zulipParser :: Parser Zulip.MessageMetadata
zulipParser = do
  instance_ <- strOption
    $  long "zulip"
    <> help "Zulip base URL (https://something.zulipchat.com)"
  user <- strOption
    $  long "zulip-user"
    <> help "Zulip bot username."
  password <- strOption
    $  long "zulip-password"
    <> help "Zulip bot password."
  stream <- strOption
    $  long "zulip-stream"
    <> help "Zulip stream where builds will be posted."
  topic <- strOption
    $  long "zulip-topic"
    <> help "Zulip topic where builds will be posted."
  pure $ Zulip.MessageMetadata
    { auth = Zulip.Auth { zulipInstance = instance_, .. }
    , destination = Zulip.MessageDestination {..}
    }

optsParser :: Parser CmdArgs
optsParser = CmdArgs
  <$> argument (maybeReader $ parseSecret . fromString)
        (  metavar "SECRET"
        <> help "A secret token shared between the supervisor and runner(s)." )
  <*> argument str
        (  metavar "DB_PASSWORD"
        <> help "Password for the Postgres database." )
  <*> option auto
        (  long "port"
        <> value _DEFAULT_PORT
        <> help "Port for the web interface." )
  <*> optional zulipParser

programInfo :: ParserInfo CmdArgs
programInfo = info optsParser
  $ progDesc "mathlib-bench benchmark supervisor"

parseCmdArgs :: IO CmdArgs
parseCmdArgs = execParser programInfo
