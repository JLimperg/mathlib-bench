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
  instance_ <- strOption $ long "zulip"
  user <- strOption $ long "zulip-user"
  password <- strOption $ long "zulip-password"
  stream <- strOption $ long "zulip-stream"
  topic <- strOption $ long "zulip-topic"
  pure $ Zulip.MessageMetadata
    { auth = Zulip.Auth { zulipInstance = instance_, .. }
    , destination = Zulip.MessageDestination {..}
    }

optsParser :: Parser CmdArgs
optsParser = CmdArgs
  <$> argument (maybeReader $ parseSecret . fromString) (metavar "SECRET")
  <*> argument str (metavar "DB_PASSWORD")
  <*> option auto (long "port" <> value _DEFAULT_PORT)
  <*> optional zulipParser

programInfo :: ParserInfo CmdArgs
programInfo = info optsParser
  $ progDesc "mathlib-bench benchmark supervisor"

parseCmdArgs :: IO CmdArgs
parseCmdArgs = execParser programInfo
