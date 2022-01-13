{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Sfdc.SObjectName
  ( SObjectName
  , sObjectName
  , Sfdc.SObjectName.toText

  -- SObjectNames
  , account
  , appTabMember
  , attachment
  , colorDefinition
  , contact
  , contentDocumentLink
  , contentFolderItem
  , opportunity
  )
where

import Data.Aeson
import Data.Char (isAlphaNum)
import Data.Text
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import Database.Persist.TH
import Servant.API

import Sfdc.HasSoqlName

newtype SObjectName = SObjectName ShortText
  deriving (Eq, Ord, Show, Read)

instance FromJSON SObjectName where
  parseJSON v = SObjectName . ST.fromText <$> parseJSON v

instance ToJSON SObjectName where
  toJSON = toJSON . toText

instance ToHttpApiData SObjectName where
  toUrlPiece = toText

sObjectName :: String -> Maybe SObjectName
sObjectName "" = Nothing
sObjectName s =
  let st = ST.pack s
  in if ST.all isAlphaNum st
     then Just $ SObjectName st
     else Nothing

toText :: SObjectName -> Text
toText (SObjectName st) = ST.toText st

instance HasSoqlName SObjectName where
  toSoqlName (SObjectName st) = ST.unpack st

account = SObjectName "Account"
appTabMember = SObjectName "AppTabMember"
attachment = SObjectName "Attachment"
colorDefinition = SObjectName "ColorDefinition"
contact = SObjectName "Contact"
contentFolderItem = SObjectName "ContentFolderItem"
contentDocumentLink = SObjectName "ContentDocumentLink"
opportunity = SObjectName "Opportunity"

-- TODO: We might want to move this somewhere else. Do we want an SFDC module to
-- depend on persistent-template?
derivePersistField "SObjectName"