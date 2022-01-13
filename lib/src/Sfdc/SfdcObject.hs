{-# LANGUAGE DeriveGeneric #-}

module Sfdc.SfdcObject
  ( SfdcObject (..)
  , sObjectName)
where

import Data.Maybe
import Generics.Deriving.ConNames (conNameOf)
import GHC.Generics

import Sfdc.HasSoqlName
import Sfdc.SObjectName (SObjectName)
import qualified Sfdc.SObjectName as SON


instance HasSoqlName SfdcObject where
  toSoqlName = conNameOf

data SfdcObject
  = AcceptedEventRelation
  | Account
  deriving (Show, Read, Eq, Ord, Generic, Enum, Bounded)

sObjectName :: SfdcObject -> SObjectName
sObjectName AcceptedEventRelation = fromJust $ SON.sObjectName "AcceptedEventRelation"
sObjectName Account = fromJust $ SON.sObjectName "Account"