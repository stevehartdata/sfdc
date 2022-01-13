{-# LANGUAGE DeriveGeneric #-}

module Sfdc.Api.Rest.DescribeGlobalResponse
  ( DescribeGlobalResponse (..)
  , allObjectNames
  , allQueryableObjects
  )
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

import Sfdc.Api.Rest.DescribeGlobalResponse.SObject (SObject)
import qualified Sfdc.Api.Rest.DescribeGlobalResponse.SObject as SObject
import Sfdc.SObjectName

data DescribeGlobalResponse = DescribeGlobalResponse
  { encoding :: Text
  , maxBatchSize :: Integer
  , sobjects :: [SObject]
  } deriving (Generic, Show)

instance FromJSON DescribeGlobalResponse

allObjectNames :: DescribeGlobalResponse -> [SObjectName]
allObjectNames = fmap SObject.name . sobjects

allQueryableObjects :: DescribeGlobalResponse -> [SObjectName]
allQueryableObjects = fmap SObject.name . filter (SObject.queryable) . sobjects