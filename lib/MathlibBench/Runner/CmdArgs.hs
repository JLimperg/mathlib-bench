module MathlibBench.Runner.CmdArgs
( CmdArgs(..)
, parseCmdArgs
) where

import           Data.String (fromString)
import           Data.Text (Text)
import           Options.Applicative

import           MathlibBench.Secret (Secret, parseSecret)

data CmdArgs = CmdArgs
  { cmdArgsRunnerId :: Text
  , cmdArgsSecret :: Secret
  }

optsParser :: Parser CmdArgs
optsParser = CmdArgs
  <$> argument str (metavar "RUNNER_ID")
  <*> argument (maybeReader $ parseSecret . fromString) (metavar "SECRET")

programInfo :: ParserInfo CmdArgs
programInfo = info optsParser
  $ progDesc "mathlib-bench benchmark runner"

parseCmdArgs :: IO CmdArgs
parseCmdArgs = execParser programInfo
