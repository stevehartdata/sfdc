{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sfdc.Api.Bulk2_0.Query.QueryRequest
  ( QueryRequest
  , queryRequest
  ) where

import Data.Aeson
import Data.Char (toLower)
import Data.Text (Text)
import GHC.Generics

import Sfdc.SoqlQuery

data QueryRequest = QueryRequest
    { operation :: Operation
    , query :: SoqlQuery
    } deriving (Show, Generic)

instance ToJSON QueryRequest

data Operation
  = Query
  | QueryAll
  deriving (Show, Generic)

instance ToJSON Operation where
  toJSON = genericToJSON defaultOptions
            { constructorTagModifier = \(x:xs) -> toLower x : xs }

-- | Create a 'QueryRequest' based on the specified SOQL query
queryRequest :: SoqlQuery -> QueryRequest
queryRequest = QueryRequest Query
