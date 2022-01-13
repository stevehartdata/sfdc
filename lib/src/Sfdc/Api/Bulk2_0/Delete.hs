{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sfdc.Api.Bulk2_0.Delete
  ( cascadeDeleteAllBlocking
  , deleteBlocking
  , deleteAllBlocking
  )
where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Extra (concatMapM)
import Data.Csv (HasHeader(..), decode, fromOnly)
import Data.Graph
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import GHC.Generics

import Sfdc.SObjectName
import Sfdc.SObjectFieldName
import Sfdc.Api.Bulk2_0.Ingest
import qualified Sfdc.Api.Bulk2_0.Ingest.CloseJobRequest as CloseJobReq
import qualified Sfdc.Api.Bulk2_0.Ingest.CreateJobRequest as CreateJobReq
import Sfdc.Api.Bulk2_0.Ingest.Batches
import qualified Sfdc.Api.Bulk2_0.Ingest.NewJobInfo as NewJobInfo
import Sfdc.Api.Bulk2_0.Ingest.Wait
import Sfdc.Api.FieldTypes.Id
import Sfdc.Api.Rest.Client (sobjectDescribe)
import Sfdc.Api.Rest.SObjectDescribe
import Sfdc.Api.Rest.SObjectDescribe.ChildRelationship
import Sfdc.Api.SfdcClient
import Sfdc.Query

-- Note: Deleting Records on SFDC
--
-- Deleting records in bulk on SFDC is somewhat complicated by the existence of
-- relationships. Objects can be related in a master-detail relationship or in a
-- "lookup" relationship. Many-to-many relationships can be created through the
-- use of "junction" objects with a pair of master-detail relationships. When
-- the master record in a master-detail relationship is deleted, so are the
-- related detail records. By contrast, deletion of a record in a lookup
-- relationship does not cascade. The result of deleting a record that is
-- referenced in a lookup relationship depends on how the lookup field is
-- configured. The lookup field can be set to be cleared when a referenced
-- object is deleted, or it can be set to disallow deletion of referenced
-- objects. If the field is configured to disallow deletion of referenced
-- records, then either the referencing record must be deleted before the
-- referenced record can be deleted, or the reference must be removed from the
-- referencing record.
--
-- Obviously, if we're deleting all of the records of a particular object type,
-- the only two possibilities are to clear the references from the lookup fields
-- or to delete the referencing records. For optional lookup fields, the
-- preference should be to clear the references. For required lookup fields, we
-- have no choice but to delete the records.
--
-- The Account object has a special field ParentId, which has the special type
-- "Hierarchy". This field works like an optional lookup field where the
-- reference is cleared if the referenced record is deleted.
--
-- Self-referencing lookup fields are not allowed to be required, nor can they
-- restrict the deletion of referenced records.
--
-- See Also:
-- <https://help.salesforce.com/articleView?id=relationships_considerations.htm&type=5>
-- <https://developer.salesforce.com/docs/atlas.en-us.object_reference.meta/object_reference/relationships_among_objects.htm>


deleteBlocking :: Session
                -> SObjectName
                -> [Id]
                -> IO ()
deleteBlocking sess target_obj xs = do
  let batches' = batches (fmap DeleteRecord xs)
  -- TODO: Allow delete jobs to run concurrently
  mapM_ (deleteBatchBlocking sess target_obj) batches'

deleteBatchBlocking :: Session
                    -> SObjectName
                    -> IngestJobBatch
                    -> IO ()
deleteBatchBlocking sess target_obj batch = do
  bracketOnError
    (do
      new_job_info <- runClientWithSession sess $
        createIngestJob $ CreateJobReq.CreateJobRequest
                        { CreateJobReq.object = target_obj
                        , CreateJobReq.operation = CreateJobReq.Delete
                        , CreateJobReq.lineEnding = CreateJobReq.CRLF
                        }
      return $ NewJobInfo.id new_job_info)
    (\job_id -> runClientWithSession sess $
      ignore' (Proxy :: Proxy InvalidJobStateException) . void $ abortJob job_id)
    (\job_id -> do
      runClientWithSession sess $ do
        submitIngestBatch job_id batch
        void $ closeIngestJob job_id $ CloseJobReq.CloseJobRequest
          { CloseJobReq.state = CloseJobReq.UploadComplete }
      waitForJobToComplete sess job_id)

ignore' :: (Exception e, MonadCatch m) => Proxy e -> m () -> m ()
ignore' (_ :: Proxy e) m = m `catch` (\(_ :: e) -> return ())

data DeleteRecord = DeleteRecord
  { id :: Id }
  deriving (Generic)

instance DefaultOrdered DeleteRecord
instance ToNamedRecord DeleteRecord

data DeleteException
  = CouldNotDecodeQueryResults
  deriving (Show)

instance Exception DeleteException

deleteAllBlocking :: Session
                  -> SObjectName
                  -> IO ()
deleteAllBlocking sess obj = do
  bss <- runBulkQuery sess (queryBulk obj (return [sObjectFieldName'|Id|]))
  ids <- concatMapM
    (either (const $ throwM CouldNotDecodeQueryResults)
            (return . fmap fromOnly . Vector.toList)
      . decode HasHeader) bss
  deleteBlocking sess obj ids

-- FIXME: Cascade delete needs to use either the Bulk API or the REST API as
-- appropriate, as not all objects are supported by the Bulk API. Currently, it
-- tries to use the Bulk API for all objects.
cascadeDeleteAllBlocking :: Session
                          -> SObjectName
                          -> IO ()
cascadeDeleteAllBlocking sess obj = do
  delete_steps <- runClientWithSession sess $ cascadeDeleteSteps obj
  mapM_ (\(DeleteAll obj_nm) -> deleteAllBlocking sess obj_nm) delete_steps

data DeletionStep
  = DeleteAll SObjectName
  -- | ClearField SObjectName SObjectFieldName

-- We need to build up a DAG of restricted delete parent-child relationships and
-- then do a topological sort to find the right delete steps.
--
-- For now, we'll just delete when doing the cascade delete, even if we could
-- instead just clear a field. This will keep things simple. We can get fancy
-- later on.
cascadeDeleteSteps :: SObjectName -> SfdcClient [DeletionStep]
cascadeDeleteSteps target_obj = do
  (RestrictedDeleteDag dag_map) <- restrictedDeleteDag target_obj
  let (g0, decodeVertex, _) =
          graphFromEdges
          . fmap (\(k, v) -> (k, k, (Set.toList v)))
          . Map.toList $ dag_map
      g = transposeG g0
  return $ fmap ((\(n, _, _) -> DeleteAll n) . decodeVertex) . topSort $ g

newtype RestrictedDeleteDag = RestrictedDeleteDag (Map SObjectName (Set SObjectName))

explore :: SObjectName -> RestrictedDeleteDag -> SfdcClient ([SObjectName], RestrictedDeleteDag)
explore obj dag@(RestrictedDeleteDag dag_map) =
  case Map.lookup obj dag_map of
    Nothing -> do
      obj_desc <- sobjectDescribe obj
      let cs = (if obj == account then [opportunity] else [])
                -- Add Opportunities as a child object, as we cannot delete an
                -- Account if it is associated with a "Closed Won" Opportunity.
                -- I don't know why, but the sobjectDescribe result does not
                -- list this relationship as RestrictedDelete.
                ++ (fmap childSObject
                    . filter (\r -> (restrictedDelete r) == Just True)
                    . childRelationships $ obj_desc)
      return (cs, RestrictedDeleteDag $ Map.insert obj (Set.fromList cs) dag_map)
    Just _ -> return ([], dag)

restrictedDeleteDag :: SObjectName -> SfdcClient RestrictedDeleteDag
restrictedDeleteDag starting_obj = go [starting_obj] (RestrictedDeleteDag mempty)
  where
    go :: [SObjectName] -> RestrictedDeleteDag -> SfdcClient RestrictedDeleteDag
    go [] dag = return dag
    go (x:xs) dag = do
      (discovered, dag') <- explore x dag
      go (discovered ++ xs) dag'



