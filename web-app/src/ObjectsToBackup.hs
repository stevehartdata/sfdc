{-# LANGUAGE TemplateHaskell #-}

module ObjectsToBackup
  ( ObjectsToBackup (..)
  )
where

import Data.Set
import Database.Persist.TH

import Sfdc.SObjectName

data ObjectsToBackup
  = AllObjects
  | TheseObjects (Set SObjectName)
  deriving (Show, Read, Eq)

derivePersistField "ObjectsToBackup"
