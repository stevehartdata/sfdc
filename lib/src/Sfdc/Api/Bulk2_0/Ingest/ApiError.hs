{-# LANGUAGE DeriveGeneric #-}

module Sfdc.Api.Bulk2_0.Ingest.ApiError
  ( ApiError
  , errorCode
  , message
  )
where

import Data.Aeson (FromJSON)
import Data.Text
import GHC.Generics

data ApiError = ApiError
  { errorCode :: Text
  , message :: Text
  }
  deriving (Generic, Show)

instance FromJSON ApiError
