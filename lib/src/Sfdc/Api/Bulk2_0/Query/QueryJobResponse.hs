{-# LANGUAGE DeriveGeneric #-}

module Sfdc.Api.Bulk2_0.Query.QueryJobResponse where

import Data.Aeson
import Data.Text
import GHC.Generics

import Sfdc.Api.Bulk2_0.Query.JobId

data QueryJobResponse = QueryJobResponse
    { id :: JobId
    , operation :: Text
    , object :: Text
    , createdById :: Text
    , createdDate :: Text
    , systemModstamp :: Text
    , state :: Text
    , concurrencyMode :: Text
    , contentType :: Text
    , apiVersion :: Float
    , lineEnding :: Text
    , columnDelimiter :: Text
    } deriving (Show, Generic)

instance FromJSON QueryJobResponse
