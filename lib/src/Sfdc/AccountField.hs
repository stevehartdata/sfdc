{-# LANGUAGE DeriveGeneric #-}

module Sfdc.AccountField
  ( AccountField (..)
  )
where

import Generics.Deriving.ConNames (conNameOf)
import GHC.Generics

import Sfdc.Api.SoqlField
import Sfdc.BulkApiSupport
import qualified Sfdc.SfdcObject as O


instance HasSoqlName AccountField where
  toSoqlName = conNameOf

instance SoqlField AccountField where
  parentObjectName = const O.Account

instance BulkApiSupport AccountField

data AccountField
  = AccountNumber
  | AccountSource
  | AnnualRevenue
  | BillingAddress
  | BillingCity
  | BillingCountry
  | BillingCountryCode
  | BillingGeocodeAccuracy
  | BillingLatitude
  | BillingLongitude
  | BillingPostalCode
  | BillingState
  | BillingStateCode
  | BillingStreet
  | ChannelProgramName
  | ChannelProgramLevelName
  | CleanStatus
  | ConnectionReceivedId
  | ConnectionSentId
  | Description
  | DunsNumber
  | Fax
  | HasOptedOutOfEmail
  | Industry
  | IsCustomerPortal
  | IsDeleted
  | IsPartner
  | IsPersonAccount
  | Jigsaw
  | LastActivityDate
  | LastReferencedDate
  | LastViewedDate
  | MasterRecordId
  | NaicsCode
  | NaicsDesc
  | Name
  | NumberOfEmployees
  | OperatingHoursId
  | OwnerId
  | Ownership
  | ParentId
  | PersonIndividualId
  | Phone
  | PhotoUrl
  | Rating
  | RecordTypeId
  | Salutation
  | ShippingAddress
  | ShippingCity
  | ShippingCountry
  | ShippingCountryCode
  | ShippingGeocodeAccuracy
  | ShippingLatitude
  | ShippingLongitude
  | ShippingPostalCode
  | ShippingState
  | ShippingStateCode
  | ShippingStreet
  | Sic
  | SicDesc
  | Site
  | TickerSymbol
  | Tradestyle
  | Type
  | Website
  | YearStarted
  deriving (Generic, Bounded, Enum)