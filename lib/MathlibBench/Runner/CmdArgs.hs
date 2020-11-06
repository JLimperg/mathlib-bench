module MathlibBench.Runner.CmdArgs
( CmdArgs(..)
, parseCmdArgs
) where

import           Data.String (fromString)
import           Options.Applicative

import           MathlibBench.Secret (Secret, parseSecret)

data CmdArgs = CmdArgs
  { cmdArgsSecret :: Secret
  }

optsParser :: Parser CmdArgs
optsParser = CmdArgs
  <$> argument (maybeReader $ parseSecret . fromString) (metavar "SECRET")

programInfo :: ParserInfo CmdArgs
programInfo = info optsParser
  $ progDesc "mathlib-bench benchmark runner"

parseCmdArgs :: IO CmdArgs
parseCmdArgs = execParser programInfo
