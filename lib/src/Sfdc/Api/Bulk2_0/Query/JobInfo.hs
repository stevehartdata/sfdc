{-# LANGUAGE DeriveGeneric #-}

module Sfdc.Api.Bulk2_0.Query.JobInfo
  ( JobInfo (..)
  , State (..)
  )
where

import Data.Aeson
import Data.Char (toUpper)
import Data.Text (Text)
import Data.Time
import GHC.Generics
import Numeric.Natural

import Sfdc.Api.ApiVersion

data JobInfo = JobInfo
  { id :: Text
  , operation :: Text
  , object :: Text
  , createdById :: Text
  , createdDate :: UTCTime
  , systemModstamp :: UTCTime
  , state :: State
  , concurrencyMode :: ConcurrencyMode
  , contentType :: ContentType
  , apiVersion :: ApiVersion
  , jobType :: JobType
  , lineEnding :: LineEnding
  , columnDelimiter :: ColumnDelimiter
  , numberRecordsProcessed :: Maybe Natural
  , retries :: Natural
  , totalProcessingTime :: Float -- FIXME: We can lose information here converting from decimal
  }
  deriving (Show, Eq, Generic)

instance FromJSON JobInfo

data State
  = UploadComplete
  | InProgress
  | Aborted
  | JobComplete
  | Failed
  deriving (Show, Eq, Generic)

instance FromJSON State
  -- parseJSON = withString "State" $ \v -> return $
  --   case v of
  --     "UploadComplete" -> UploadComplete
  --     "InProgress" -> InProgress
  --     "Aborted" -> Aborted
  --     "JobComplete" -> JobComplete
  --     "Failed" -> Failed

data ConcurrencyMode = Parallel deriving (Show, Eq, Generic)

instance FromJSON ConcurrencyMode where
  parseJSON =
    genericParseJSON
    $ defaultOptions {tagSingleConstructors = True}

data ContentType = Csv deriving (Show, Eq, Generic)

instance FromJSON ContentType where
  parseJSON =
    genericParseJSON
    $ defaultOptions { tagSingleConstructors = True
                     , constructorTagModifier = map toUpper
                     }

data JobType = V2Query deriving (Show, Eq, Generic)

instance FromJSON JobType where
  parseJSON =
    genericParseJSON
    $ defaultOptions {tagSingleConstructors = True}

data LineEnding
  = Lf
  | Crlf
  deriving (Show, Eq, Generic)

instance FromJSON LineEnding where
  parseJSON =
    genericParseJSON
    $ defaultOptions {constructorTagModifier = map toUpper}

data ColumnDelimiter
  = BackQuote
  | Caret
  | Comma
  | Pipe
  | Semicolon
  | Tab
  deriving (Show, Eq, Generic)

instance FromJSON ColumnDelimiter where
  parseJSON =
    genericParseJSON
    $ defaultOptions {constructorTagModifier = map toUpper}
