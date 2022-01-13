{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.BackupFileLocator
  ( BackupFileLocator (..)
  , BackupFileExtension (..)
  , backupFileExtensionFromText
  )
where

import ClassyPrelude.Yesod   as Import
import qualified Data.Text as T
import Numeric.Natural
import Text.Printf (printf)
import Text.Read (readMaybe)

import Sfdc.SObjectName (SObjectName, sObjectName)
import qualified Sfdc.SObjectName as SObjectName

data BackupFileLocator = BackupFileLocator
  { objectName :: SObjectName
  , fileNumber :: Natural
  , fileExtension :: BackupFileExtension
  }
  deriving (Show, Read, Eq)

instance PathPiece BackupFileLocator where
  fromPathPiece t =
    case T.split (`elem` ['-', '.']) t of
      [obj, n, ext] ->
        BackupFileLocator
          <$> (sObjectName . T.unpack $ obj)
          <*> (readMaybe . T.unpack $ n)
          <*> backupFileExtensionFromText ext
      _ -> Nothing
  toPathPiece (BackupFileLocator obj n ext) =
    SObjectName.toText obj
    <> "-"
    <> (T.pack $ printf "%d" n)
    <> "."
    <> (case ext of CsvExt -> "csv"; JsonExt -> "json")

data BackupFileExtension
  = CsvExt
  | JsonExt
  deriving (Show, Read, Eq)

backupFileExtensionFromText :: Text -> Maybe BackupFileExtension
backupFileExtensionFromText "csv" = Just $ CsvExt
backupFileExtensionFromText "json" = Just $ JsonExt
backupFileExtensionFromText _ = Nothing
