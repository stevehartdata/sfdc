module Sfdc.Api.SoqlField
  ( SoqlField (..)
  , HasSoqlName (..))
where

import Data.Proxy (Proxy)

import Sfdc.HasSoqlName
import Sfdc.SfdcObject


class HasSoqlName a => SoqlField a where
  parentObjectName :: Proxy a -> SfdcObject

