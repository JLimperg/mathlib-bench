{-# LANGUAGE OverloadedStrings #-}

module MathlibBench.Command
(cmd, cmd_)
where

import           Control.Monad (void)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL
import           System.Exit (exitFailure, ExitCode(..))
import           System.Process.Typed (readProcess, proc)

import           MathlibBench.Logging

cmd :: String -> [String] -> IO (BL.ByteString, BL.ByteString)
cmd prog args = do
  (exitcode, stdout, stderr) <- readProcess $ proc prog args
  case exitcode of
    ExitSuccess -> pure (stdout, stderr)
    ExitFailure n -> do
      logError "external command failed"
      logError $ TL.unwords $ "command:" : TL.pack prog : map TL.pack args
      logError $ "exit code: " <> TL.pack (show n)
      logError "stdout:"
      TL.putStrLn $ TL.decodeUtf8 stdout
      logError "stderr:"
      TL.putStrLn $ TL.decodeUtf8 stderr
      exitFailure

cmd_ :: String -> [String] -> IO ()
cmd_ prog args = void $ cmd prog args
