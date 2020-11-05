{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MathlibBench.UnixSeconds
( UnixSeconds
, getUnixSeconds
) where

import           Data.Int (Int64)
import           Data.Time.Clock (nominalDiffTimeToSeconds)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Database.SQLite.Simple.FromField (FromField(..))
import           Database.SQLite.Simple.ToField (ToField(..))

newtype UnixSeconds = UnixSeconds { secondsFromEpoch :: Int64 }
  deriving (Num, Eq, Ord)

instance FromField UnixSeconds where
  fromField = fmap UnixSeconds . fromField

instance ToField UnixSeconds where
  toField = toField . secondsFromEpoch

getUnixSeconds :: IO UnixSeconds
getUnixSeconds = UnixSeconds . round . nominalDiffTimeToSeconds <$> getPOSIXTime
