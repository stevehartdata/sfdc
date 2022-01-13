{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Sfdc.Api.Bulk2_0.Ingest.IngestJobBatch
  ( IngestJobBatch (..)
  , ingestJobBatch

  , DefaultOrdered
  , ToNamedRecord
  )
where

import Data.ByteString.Lazy
import Data.Csv (DefaultOrdered, ToNamedRecord, encodeDefaultOrderedByName)
import Servant.API.ContentTypes

import Sfdc.Api.Bulk2_0.Ingest.Csv

newtype IngestJobBatch = IngestJobBatch ByteString

ingestJobBatch :: (DefaultOrdered a, ToNamedRecord a) => [a] -> IngestJobBatch
ingestJobBatch = IngestJobBatch . encodeDefaultOrderedByName 

instance MimeRender Csv IngestJobBatch where
  mimeRender _ (IngestJobBatch bs) = bs