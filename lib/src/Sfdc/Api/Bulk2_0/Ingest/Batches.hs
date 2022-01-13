{-# LANGUAGE ScopedTypeVariables #-}

module Sfdc.Api.Bulk2_0.Ingest.Batches
  ( batches
  , DefaultOrdered
  , ToNamedRecord
  )
where

import ByteString.TreeBuilder as TreeBuilder
import Data.Csv (Header, headerOrder)
import Data.Csv.TreeBuilder (encodeHeader, encodeNamedRecord)
import Data.Functor ((<&>))

import Sfdc.Api.Bulk2_0.Ingest.IngestJobBatch

-- "A request can provide CSV data that does not in total exceed 150 MB of
-- base64 encoded content. When job data is uploaded, it is converted to
-- base64. This conversion can increase the data size by approximately 50%. To
-- account for the base64 conversion increase, upload data that does not exceed
-- 100 MB."
-- https://developer.salesforce.com/docs/atlas.en-us.api_bulk_v2.meta/api_bulk_v2/upload_job_data.htm
batches :: forall record. (DefaultOrdered record, ToNamedRecord record)
        => [record] -> [IngestJobBatch]
batches [] = mempty
batches records@(r1:_) =
  let header = headerOrder r1
  in
    go header records (encodeHeader header)
    <&> (IngestJobBatch . TreeBuilder.toLazyByteString)
  where
    go :: Header -> [record] -> Builder -> [Builder]
    go header (x:xs) builder =
      let new_builder = encodeNamedRecord header x
      in
        if TreeBuilder.length builder + TreeBuilder.length new_builder > batchSizeLimit
          then builder : go header xs (encodeHeader header <> new_builder)
          else go header xs (builder <> new_builder)
    go _ [] builder = [builder]

batchSizeLimit = 100000000 -- 100 MB
