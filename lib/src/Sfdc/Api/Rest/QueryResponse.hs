{-# LANGUAGE DeriveGeneric #-}

module Sfdc.Api.Rest.QueryResponse
  ( QueryResponse (..)
  )
where

import Data.Aeson
import Data.Text
import GHC.Generics

data QueryResponse = QueryResponse
  { done :: Bool
  , totalSize :: Integer
  , records :: [Object]
  , nextRecordsUrl :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON QueryResponse
