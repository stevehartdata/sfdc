{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Sfdc.SObjectFieldName
  ( SObjectFieldName
  , sObjectFieldName
  , sObjectFieldName'
  , Sfdc.SObjectFieldName.toText
  )
where

import Data.Char (isAlphaNum)
import Data.Aeson
import Data.Maybe (maybe)
import Data.Text as T
import GHC.Generics
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (Lift, lift)

import Sfdc.HasSoqlName

newtype SObjectFieldName = SObjectFieldName Text
  deriving (Eq, Ord, Show, Generic, Lift)

sObjectFieldName :: String -> Maybe SObjectFieldName
sObjectFieldName s =
  let st = T.pack s
  in if T.all isAlphaNum st
     then Just $ SObjectFieldName st
     else Nothing

sObjectFieldName' :: QuasiQuoter
sObjectFieldName' = QuasiQuoter
  { quoteExp =
      maybe (fail "Invalid SObjectFieldName") (lift) . sObjectFieldName
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

toText :: SObjectFieldName -> Text
toText (SObjectFieldName t) = t

instance HasSoqlName SObjectFieldName where
  toSoqlName (SObjectFieldName t) = T.unpack t

instance FromJSON SObjectFieldName where
  parseJSON = fmap SObjectFieldName . parseJSON
