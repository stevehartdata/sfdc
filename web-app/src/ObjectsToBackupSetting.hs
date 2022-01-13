{-# LANGUAGE TemplateHaskell #-}

module ObjectsToBackupSetting
where

import ClassyPrelude.Yesod
import Database.Persist.TH

import Sfdc.SfdcObject


data ObjectsToBackupSetting
  = AllObjects
  | SpecifiedObjects
  deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "ObjectsToBackupSetting"
