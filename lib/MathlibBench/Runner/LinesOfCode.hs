{-# LANGUAGE OverloadedStrings #-}

module MathlibBench.Runner.LinesOfCode
( countLinesOfCode
) where

import           Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           System.Exit (exitFailure)

import           MathlibBench.Command (cmd)
import           MathlibBench.Logging (logError)
import           MathlibBench.Types (LinesOfCode(..))

newtype ClocOutput = ClocOutput { clocOutputlinesOfCode :: LinesOfCode }

instance JSON.FromJSON ClocOutput where
  parseJSON =
    JSON.withObject "ClocOutput" $ \v -> do
      lean <- v .: "Lean"
      flip (JSON.withObject "Lean") lean $ \v' -> do
        ClocOutput <$> v' .: "code"

countLinesOfCode :: FilePath -> IO LinesOfCode
countLinesOfCode file = do
  (stdout, stderr) <- cmd "cloc" ["--json", file]
  case JSON.eitherDecode' stdout of
    Right (ClocOutput loc) -> pure loc
    Left err -> do
      logError $
        "failed to parse cloc output for Lean file '" <>
        TL.pack file <>
        "'.\ncloc stdout:\n" <>
        TL.decodeUtf8 stdout <>
        "\ncloc stderr:\n" <>
        TL.decodeUtf8 stderr <>
        "\nJSON decoding error:\n" <>
        TL.pack err
      exitFailure
