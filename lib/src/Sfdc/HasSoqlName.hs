module Sfdc.HasSoqlName
  (HasSoqlName (..))
where

class HasSoqlName a where
  toSoqlName :: a -> String

