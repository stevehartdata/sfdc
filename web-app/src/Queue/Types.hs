module Queue.Types (BackupQueue (..)) where

import Import.NoFoundation

import Database.Persist.Sql (ConnectionPool)

data BackupQueue = BackupQueue
  { chan :: Chan (QueuedBackupJobId, QueuedBackupJob)
  , connectionPool :: ConnectionPool
  }
