{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.QueuedBackupJob
  ( getQueuedBackupJobR
  , getDownloadBackupFileR
  )
where

import qualified Data.ByteString as BS
import Data.Char (isAlphaNum)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time.Format.ISO8601 (iso8601Show)
import Import
import System.Directory (listDirectory)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Model.BackupFileLocator (BackupFileLocator (BackupFileLocator))
import qualified Model.BackupFileLocator as BackupFileLocator
import Sfdc.SObjectName (SObjectName, sObjectName)
import qualified Sfdc.SObjectName as SObjectName

getQueuedBackupJobR :: Key BackupJob -> Key QueuedBackupJob -> Handler Html
getQueuedBackupJobR bj_id qbj_id = do
  qbj <- runDB $ get404 qbj_id

  checkUserId $ queuedBackupJobUserId qbj

  backup_files <- Map.toAscList <$> backupFiles qbj_id

  defaultLayout $(widgetFile "queued-backup-job")

getDownloadBackupFileR :: Key BackupJob -> Key QueuedBackupJob -> BackupFileLocator
                    -> Handler (ContentType, Content)
getDownloadBackupFileR _ qbj_id loc = do
  qbj <- runDB $ get404 qbj_id

  checkUserId $ queuedBackupJobUserId qbj

  backup_dir <- backupFileDir
  file_content <- catchJust
    (guard . isDoesNotExistError)
    (liftIO $ BS.readFile
      (backup_dir </> show qbj_id </> backupFileLocatorToSafePath loc))
    (const $ notFound)
  return (typeOctet, toContent file_content)

backupFileLocatorToSafePath :: BackupFileLocator -> FilePath
backupFileLocatorToSafePath loc =
  let
    obj = T.unpack . SObjectName.toText . BackupFileLocator.objectName $ loc
    n = printf "%d" . BackupFileLocator.fileNumber $ loc
    ext = case BackupFileLocator.fileExtension loc of
            BackupFileLocator.CsvExt -> "csv"
            BackupFileLocator.JsonExt -> "json"
  in if all (isAlphaNum) obj
      then obj </> n <.> ext
      else error "SObjectName unexpectedly contained a non-alphanumeric character"

backupFiles :: Key QueuedBackupJob
            -> Handler (Map SObjectName [BackupFileLocator])
backupFiles qbj_id = do
  backup_dir <- backupFileDir


  liftIO $ do
    objs <- liftIO $ listDirectory $ backup_dir </> show qbj_id

    Map.fromList
      <$> mapM (\obj_nm -> do
              let sobj_nm = fromMaybe (error "Invalid SObjectName")
                            $ sObjectName obj_nm
              files <- listDirectory $ backup_dir </> show qbj_id </> obj_nm
              let locs = fmap (fileNameToLocator sobj_nm) files
              return (sobj_nm, locs))
            objs

fileNameToLocator :: SObjectName -> FilePath -> BackupFileLocator
fileNameToLocator sobj_nm fname =
  case T.split (== '.') . T.pack $ fname of
    [n_txt, ext_txt] ->
      case readMaybe . T.unpack $ n_txt of
        Just n -> BackupFileLocator sobj_nm n $
          case backupFileExtensionFromText ext_txt of
            Just ext -> ext
            Nothing -> error "Invalid file extension"
        Nothing -> error "File number could not be parsed"
    _ -> error "Invalid file name"
