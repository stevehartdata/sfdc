{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Sfdc.Api.Bulk2_0.Ingest.Csv
  ( Csv
  )
where

import Data.Csv (encDelimiter)
import Data.Proxy
import Network.HTTP.Media.MediaType ((//))
import Servant.CSV.Cassava

data SfdcOpts = SfdcOpts

instance EncodeOpts SfdcOpts where
  encodeOpts _ = encodeOpts (Proxy :: Proxy DefaultOpts)
  -- SFDC does not support specifying the charset, which the DefaultOpts do.
  csvContentType p = case encDelimiter (encodeOpts p) of
    -- ord '\t' = 9
    9 -> "text" // "tab-separated-values"
    _ -> "text" // "csv"

type Csv = CSV' 'HasHeader SfdcOpts
