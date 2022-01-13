{-# LANGUAGE TemplateHaskell #-}

module BackupJobStatus
  ( BackupJobStatus (..)
  )
where

import Data.Time.Clock (UTCTime)
import Database.Persist.TH

data BackupJobStatus
  = Queued UTCTime
  | Started UTCTime
  | Ended UTCTime
  | Failed UTCTime String
  deriving (Eq, Show, Read)

derivePersistField "BackupJobStatus"
