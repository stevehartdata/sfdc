{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Queue
  ( BackupQueue
  , enqueue
  , dequeue
  , newBackupQueue
  )
where

import qualified Data.Set as Set
import Import.NoFoundation

import BackupJobStatus as BJS
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import ObjectsToBackup
import qualified ObjectsToBackupSetting as ObjectsToBackupSetting
import Queue.Types (BackupQueue (BackupQueue))
import qualified Queue.Types as BackupQueue

-- TODO: Load queue from DB, update status on any queued items that were in
-- progress when another instance of the queue crashed.
newBackupQueue :: ConnectionPool -> IO BackupQueue
newBackupQueue connection_pool = do
  chan <- newChan
  return $ BackupQueue chan connection_pool

enqueue :: BackupQueue -> Entity BackupJob -> IO QueuedBackupJobId
enqueue queue bj_entity = do
  let c = BackupQueue.chan queue

  (k, qbj) <- flip runSqlPool (BackupQueue.connectionPool queue) $ do
    qbj <- backupJobToQueuedBackupJob bj_entity
    k <- insert qbj
    return (k, qbj)
  writeChan c (k, qbj)
  return k

dequeue :: BackupQueue -> IO (QueuedBackupJobId, QueuedBackupJob)
dequeue queue = do
  let c = BackupQueue.chan queue

  now <- getCurrentTime
  res@(qbj_id, _) <- readChan c
  flip runSqlPool (BackupQueue.connectionPool queue)
    $ update qbj_id [QueuedBackupJobStartedAt =. Just now]
  return res

backupJobToQueuedBackupJob :: ( PersistRecordBackend QueuedBackupJob backend
                              , PersistRecordBackend User backend
                              , PersistStoreRead backend
                              , PersistStoreWrite backend
                              , MonadIO m
                              )
                           => Entity BackupJob -> ReaderT backend m QueuedBackupJob
backupJobToQueuedBackupJob bj_entity = do
  let bj = entityVal bj_entity
  u <- (fromMaybe (error "Could not get user info"))
        <$> get (backupJobUserId bj)
  let objects = case backupJobObjectsToBackupSetting bj of
        ObjectsToBackupSetting.AllObjects -> AllObjects
        ObjectsToBackupSetting.SpecifiedObjects ->
          TheseObjects . Set.fromList $ backupJobSelectedObjects bj
  now <- liftIO getCurrentTime
  return $ QueuedBackupJob
    (backupJobUserId bj)
    (userSfdcPreferredUsername u)
    (Just $ entityKey bj_entity)
    objects
    now
    Nothing
    Nothing
    Nothing
    Nothing