{-# LANGUAGE TemplateHaskell #-}

module MathlibBench.Supervisor.Frontend.Static
( globalCss
) where

import           Data.FileEmbed
import           Data.ByteString (ByteString)

globalCss :: ByteString
globalCss = $(makeRelativeToProject "static/global.css" >>= embedFile)
