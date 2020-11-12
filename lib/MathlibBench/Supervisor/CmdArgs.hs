module MathlibBench.Supervisor.CmdArgs
( CmdArgs(..)
, parseCmdArgs
) where

import           Data.String (fromString)
import           Options.Applicative

import           MathlibBench.Secret (Secret, parseSecret)

data CmdArgs = CmdArgs
  { cmdArgsSecret :: Secret
  , cmdArgsDatabasePassword :: String
  }

optsParser :: Parser CmdArgs
optsParser = CmdArgs
  <$> argument (maybeReader $ parseSecret . fromString) (metavar "SECRET")
  <*> argument str (metavar "DB_PASSWORD")

programInfo :: ParserInfo CmdArgs
programInfo = info optsParser
  $ progDesc "mathlib-bench benchmark supervisor"

parseCmdArgs :: IO CmdArgs
parseCmdArgs = execParser programInfo
