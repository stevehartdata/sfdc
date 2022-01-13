{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.BackupJob where

import Data.Time.Format.ISO8601 (iso8601Show)
import Import hiding (fieldSettingsLabel, radioField, checkboxesField)
import qualified Import as I
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4, radioField, checkboxesField, fieldSettingsLabel)

import qualified Model.BackupJob as BackupJob
import ObjectsToBackupSetting
import ScheduledDays (ScheduledDays (ScheduledDays))
import qualified ScheduledDays
import ScheduledTimes (ScheduledTimes (ScheduledTimes))
import qualified ScheduledTimes
import Sfdc.Api.Rest.Client (describeGlobal)
import Sfdc.Api.Rest.DescribeGlobalResponse
import Sfdc.Api.SfdcClient (runClientWithSession)
import Sfdc.SObjectName (SObjectName)
import qualified Sfdc.SObjectName as SObjectName
import Sfdc.SfdcObject

getEditBackupJobR :: Key BackupJob -> Handler Html
getEditBackupJobR = getEditBackupJobR' . Just

getEditBackupJobR' :: Maybe (Key BackupJob) -> Handler Html
getEditBackupJobR' mb_backup_job_id = do
  mb_backup_job <- flip mapM mb_backup_job_id $ \backup_job_id -> do
    backup_job <- runDB . get404 $ backup_job_id
    checkUserId $ backupJobUserId backup_job
    return backup_job

  (formWidget, formEnctype) <- generateFormPost
    $ backupJobForm mb_backup_job
  defaultLayout $(widgetFile "submit-backup-job")


postEditBackupJobR :: Key BackupJob -> Handler Html
postEditBackupJobR = postEditBackupJobR' . Just

postEditBackupJobR' :: Maybe (Key BackupJob) -> Handler Html
postEditBackupJobR' mb_backup_job_id = do
  checkCsrfParamNamed defaultCsrfParamName

  mb_action <- lookupPostParam "action"
  case mb_action of
    Just "refresh_objects" -> void $ refreshAccessibleObjects
    _ -> return ()

  ((res, formWidget), formEnctype) <- runFormPost
    $ backupJobForm Nothing
  case (res, mb_action) of
    (FormSuccess bj, Nothing) -> do
      bj_entity <-
        case mb_backup_job_id of
          Just backup_job_id -> runDB $ do
            backup_job <- get404 $ backup_job_id
            lift $ checkUserId $ backupJobUserId backup_job

            replace backup_job_id bj >> getJustEntity backup_job_id
          Nothing ->
            runDB $ insertEntity bj
      enqueue bj_entity
      redirect $ BackupJobR $ entityKey bj_entity
    _ -> do
      defaultLayout $(widgetFile "submit-backup-job")


getNewBackupJobR :: Handler Html
getNewBackupJobR = getEditBackupJobR' Nothing

getAccessibleObjects :: Key User -> Handler [SObjectName]
getAccessibleObjects user_id = do
  mb_res <- runDB $ getBy $ UniqueUserAccessibleObjects user_id
  case mb_res of
    Nothing -> refreshAccessibleObjects
    Just res -> return $ userAccessibleObjectsObjects . entityVal $ res

accessibleObjectsOptionList :: Handler (OptionList SObjectName)
accessibleObjectsOptionList = do
  user_id <- requireAuthId
  accessible_objs <- getAccessibleObjects user_id
  optionsPairs . fmap (\nm -> (SObjectName.toText nm, nm)) $ accessible_objs

refreshAccessibleObjects :: Handler [SObjectName]
refreshAccessibleObjects = do
  user <- requireAuth
  let username = userSfdcPreferredUsername . entityVal $ user
      user_id = entityKey user
  sess <- newSfdcSession username
  objs <- liftIO $ runClientWithSession sess (allObjectNames <$> describeGlobal)
  void $ runDB $ upsertBy
    (UniqueUserAccessibleObjects user_id)
    (UserAccessibleObjects user_id objs)
    [UserAccessibleObjectsObjects =. objs]
  return objs

postNewBackupJobR :: Handler Html
postNewBackupJobR = postEditBackupJobR' Nothing

backupJobForm :: Maybe BackupJob -> Form BackupJob
backupJobForm mb_bj = renderBootstrap4 BootstrapBasicForm $
  backupJob
    <$> areq textField (fieldSettingsLabel ("Backup Job Name" :: Text)) (backupJobName <$> mb_bj)
    <*> lift requireAuthId
    <*> areq 
          (radioField
            (optionsPairs
              [ ("Backup all objects" :: Text, AllObjects)
              , ("Backup only the objects specified below", SpecifiedObjects)
              ]))
          (fieldSettingsLabel ("Backup Mode" :: Text)) (backupJobObjectsToBackupSetting <$> mb_bj)
    <*> (fromMaybe []
         <$> aopt
              (checkboxesField accessibleObjectsOptionList)
              (fieldSettingsLabel ("Objects to Backup" :: Text))
              (Just . backupJobSelectedObjects <$> mb_bj))
    <*> (maybe ScheduledDays.none ScheduledDays.fromList
         <$> aopt
              (checkboxesField optionsEnum)
              "Run on"
              (Just . ScheduledDays.toList . BackupJob.scheduledDays <$> mb_bj)) -- FIXME: Populate from DB
    <*> (maybe ScheduledTimes.none ScheduledTimes.fromList
         <$> aopt
              (checkboxesField optionsEnum)
              "Run at"
              (Just. ScheduledTimes.toList . BackupJob.scheduledTimes <$> mb_bj)) -- FIXME: Populate from DB

backupJob :: Text -> UserId -> ObjectsToBackupSetting -> [SObjectName]
          -> ScheduledDays -> ScheduledTimes -> BackupJob
backupJob name user_id objs_to_backup selected_objs days times =
  let (ScheduledDays m t w r f s) = days
      (ScheduledTimes h0 h1 h2 h3 h4 h5 h6 h7 h8 h9 h10 h11 h12
        h13 h14 h15 h16 h17 h18 h19 h20 h21 h22 h23) = times
  in BackupJob name user_id objs_to_backup selected_objs
      m t w r f s
      h0 h1 h2 h3 h4 h5 h6 h7 h8 h9 h10 h11 h12
      h13 h14 h15 h16 h17 h18 h19 h20 h21 h22 h23

getBackupJobsR :: Handler Html
getBackupJobsR = do
  user_id <- requireAuthId
  jobs <- runDB $ selectList [BackupJobUserId ==. user_id] []
  defaultLayout $(widgetFile "backup-jobs")

getBackupJobR :: Key BackupJob -> Handler Html
getBackupJobR job_id = do
  (job, queued_jobs) <- runDB $ do
    job <- get404 job_id
    queued_jobs <- selectList
                    [QueuedBackupJobBackupJob ==. Just job_id]
                    [Desc QueuedBackupJobQueuedAt]
    return (job, queued_jobs)
  checkUserId $ backupJobUserId job
  defaultLayout $(widgetFile "backup-job")
