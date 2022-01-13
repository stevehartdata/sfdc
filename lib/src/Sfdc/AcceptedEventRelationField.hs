{-# LANGUAGE DeriveGeneric #-}

module Sfdc.AcceptedEventRelationField
  ( AcceptedEventRelationField (..)
  )
where

import Generics.Deriving.ConNames (conNameOf)
import GHC.Generics

import Sfdc.Api.SoqlField
import qualified Sfdc.SfdcObject as O


instance HasSoqlName AcceptedEventRelationField where
  toSoqlName = conNameOf

instance SoqlField AcceptedEventRelationField where
  parentObjectName = const O.AcceptedEventRelation

data AcceptedEventRelationField
  = EventId
  | RelationId
  | RespondedDate
  | Response
  | Type
  deriving (Generic, Bounded, Enum)
