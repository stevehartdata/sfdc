{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Sfdc.SoqlQuery
  ( SoqlQuery
  , buildQuery
  , soqlQueryFromText
  )
where

import Data.Aeson
import Data.Text as T
import Data.List.NonEmpty as NE
import Servant.API (ToHttpApiData)

import Sfdc.HasSoqlName
import Sfdc.SObjectFieldName (SObjectFieldName)
import Sfdc.SObjectName (SObjectName)

-- | A SOQL Query
newtype SoqlQuery = SoqlQuery T.Text
  deriving (Show, ToHttpApiData, ToJSON)

buildQuery :: SObjectName -> NonEmpty SObjectFieldName -> SoqlQuery
buildQuery obj_nm field_nms = SoqlQuery $
  "SELECT "
  <> (T.intercalate ", " . NE.toList . fmap (T.pack . toSoqlName) $ field_nms)
  <> " FROM "
  <> (T.pack . toSoqlName $ obj_nm)

soqlQueryFromText :: Text -> SoqlQuery
soqlQueryFromText = SoqlQuery