{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sfdc.Query
  ( query
  , queryAllFields
  , queryBulk
  , queryBulkAllFields
  , queryToFilesystem

  , runBulkQuery
  , runQuery
  
  , AccountField (..)
  , QueryException
  )
where

import Control.Exception
import Control.Concurrent (threadDelay)
import Control.Monad.Catch
import Control.Monad.Extra
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Data.Aeson (Object, encodeFile)
import Data.Bifunctor
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Proxy
import Numeric.Natural
import Servant.API
import Servant.Client (ClientEnv, ClientError, ClientM)
import qualified Servant.Client (runClientM)
import System.FilePath

import Sfdc.AccountField
import Sfdc.AcceptedEventRelationField
import Sfdc.Api.SoqlField
import Sfdc.Api.Bulk2_0.Query.Client hiding (queryJobResults)
import qualified Sfdc.Api.Bulk2_0.Query.Client as Bulk (queryJobResults)
import qualified Sfdc.Api.Bulk2_0.Query.QueryJobResponse as QJR
import Sfdc.Api.Bulk2_0.Query.Wait (waitForJobToComplete)
import Sfdc.Api.Bulk2_0.Query.JobId
import qualified Sfdc.Api.Bulk2_0.Query.JobInfo as QJS
import Sfdc.Api.Bulk2_0.Query.Wait
import Sfdc.Api.Rest.Client (sobjectDescribe)
import qualified Sfdc.Api.Rest.Client as Rest
import qualified Sfdc.Api.Rest.QueryResponse as RQueryResponse
import qualified Sfdc.Api.Rest.SObjectDescribe as SObjectDescribe
import Sfdc.Api.Rest.SObjectDescribe.FieldInfo (FieldInfo)
import qualified Sfdc.Api.Rest.SObjectDescribe.FieldInfo as FieldInfo
import Sfdc.Api.Session
import Sfdc.Api.SfdcClient (SfdcClient)
import qualified Sfdc.Api.SfdcClient as SfdcClient
import Sfdc.Api.OAuth2 (AccessToken)
import Sfdc.BulkApiSupport
import Sfdc.SObjectName as SObjectName
import Sfdc.SObjectFieldName as SObjectFieldName
import Sfdc.SoqlQuery

query :: SObjectName -> NonEmpty SObjectFieldName -> QueryJob
query obj fields = QueryJob $
  ifM (supportedByBulkApi obj)
    (toQueryResults $ queryBulk' obj fields)
    (toQueryResults' $ queryRest' obj fields)

toQueryResults :: forall a. QueryStep ByteString a a -> QueryStep QueryResults a a
toQueryResults (QueryStep st) = QueryStep $ do
  env <- get
  let env' = env
        { qjeStoreFn = fmap mapStoreFn $ qjeStoreFn env }
  liftIO $ evalStateT st env'
  where
    mapStoreFn :: StoreFn QueryResults a -> StoreFn ByteString a
    mapStoreFn (StoreFn f) = StoreFn $ fmap (second (fmap mapStoreFn)) . f . CsvFile


toQueryResults' :: QueryStep [Object] a a -> QueryStep QueryResults a a
toQueryResults' (QueryStep st) = QueryStep $ do
  env <- get
  let env' = env
        { qjeStoreFn = fmap mapStoreFn $ qjeStoreFn env }
  liftIO $ evalStateT st env'
  where
    mapStoreFn :: StoreFn QueryResults a -> StoreFn [Object] a
    mapStoreFn (StoreFn f) = StoreFn $ fmap (second (fmap mapStoreFn)) . f . JsonObjects

queryAllFields :: SObjectName -> QueryJob
queryAllFields obj = QueryJob $
  ifM (supportedByBulkApi obj)
    (toQueryResults $ queryBulkAllFields' obj)
    (toQueryResults' $ queryRestAllFields' obj)

supportedByBulkApi :: SObjectName -> QueryStep query_result_type a Bool
supportedByBulkApi obj_nm = do
  desc <- runClient $ sobjectDescribe obj_nm
  let key_prefix = SObjectDescribe.keyPrefix desc
      queryable = SObjectDescribe.queryable desc
  return $ isJust key_prefix && queryable
            && not (obj_nm `elem` objectsNotSupportedByBulkApi)
  where
    objectsNotSupportedByBulkApi = [attachment]


getResults :: JobId -> QueryStep ByteString a a
getResults job_id = do
  sess <- getSession
  liftIO $ waitForJobToComplete sess job_id
  queryJobResults job_id

data QueryResults
  = CsvFile ByteString
  | JsonObjects [Object]

newtype StoreFn input a = StoreFn (input -> IO (a, Maybe (StoreFn input a)))

newtype QueryJob = QueryJob (forall a. QueryStep QueryResults a a)

newtype QueryStep query_result_type store_result b
  = QueryStep (StateT (QueryJobEnv query_result_type store_result) IO b)
  deriving (Applicative, Functor, Monad, MonadIO, MonadThrow)

data QueryJobEnv query_result_type a = QueryJobEnv
  { qjeSfdcSession :: Session
  , qjeStoreFn :: Maybe (StoreFn query_result_type a)
  }

data QueryException
  = OtherException String
  deriving (Show)

instance Exception QueryException

getSession :: QueryStep query_result_type a Session
getSession = QueryStep . fmap qjeSfdcSession $ get

store :: query_result_type -> QueryStep query_result_type a a
store query_results = QueryStep $ do
  env <- get
  let m_store_fn = qjeStoreFn env
  (r, m_store_next) <- lift $ case m_store_fn of
    Nothing -> throwM $ OtherException "No StoreFn"
    Just (StoreFn store_fn) -> liftIO $ store_fn query_results
  put env { qjeStoreFn = m_store_next }
  return r

queryToFilesystem :: Session
                   -> FilePath
                   -> QueryJob
                   -> IO ()
queryToFilesystem sess dir (QueryJob (QueryStep st)) = do
  let store_fn i = StoreFn $ \query_results -> do
        let fpath = dir </> (show i)
        case query_results of
          CsvFile bs ->
            BS.writeFile (fpath <.> "csv") bs
          JsonObjects obj ->
            encodeFile (fpath <.> "json") obj
        return ((), Just $ store_fn (i+1))
  let env = QueryJobEnv sess (Just $ store_fn 1)
  evalStateT st env

runQuery :: Session
          -> QueryJob
          -> IO [QueryResults]
runQuery sess (QueryJob (QueryStep st)) = do
  let store_fn xs = StoreFn $ \res ->
        let xs' = xs ++ [res]
        in return (xs', Just $ store_fn xs')
      env = QueryJobEnv sess (Just $ store_fn [])
  evalStateT st env

newtype BulkQueryJob = BulkQueryJob (forall a. QueryStep ByteString a a)

runBulkQuery :: Session -> BulkQueryJob -> IO [ByteString]
runBulkQuery sess (BulkQueryJob (QueryStep st)) = do
  let store_fn xs = StoreFn $ \bs ->
        let xs' = xs ++ [bs]
        in return (xs', Just $ store_fn xs')
      env = QueryJobEnv sess (Just $ store_fn [])
  evalStateT st env

-- Compound Fields should be excluded, because we will query their primitive
-- components. Compound Fields are not supported by the Bulk Query API.
nonCompoundFields :: [FieldInfo] -> [SObjectFieldName]
nonCompoundFields =
  fmap FieldInfo.name
  . filter (not . flip elem ["address", "location"] . FieldInfo._type)

queryBulk :: SObjectName
           -> NonEmpty SObjectFieldName
           -> BulkQueryJob
queryBulk obj_nm field_nms = BulkQueryJob $ queryBulk' obj_nm field_nms

queryBulk' :: SObjectName
            -> NonEmpty SObjectFieldName
            -> QueryStep ByteString a a
queryBulk' obj_nm field_nms = do
  let query_request =
        queryRequest $ buildQuery obj_nm field_nms
  qjr <- runClient $ submitQueryJob query_request
  getResults (QJR.id qjr)

queryBulkAllFields :: SObjectName -> BulkQueryJob
queryBulkAllFields obj_nm = BulkQueryJob $ queryBulkAllFields' obj_nm

queryBulkAllFields' :: SObjectName -> QueryStep ByteString a a
queryBulkAllFields' obj_nm = do
  fields <- (NE.fromList . nonCompoundFields . NE.toList
              . SObjectDescribe.fields)
              <$> runClient (sobjectDescribe obj_nm)
  queryBulk' obj_nm fields

queryRest :: SObjectName -> NonEmpty SObjectFieldName -> QueryJob
queryRest obj_nm field_nms = QueryJob $ toQueryResults' $ queryRest' obj_nm field_nms

queryRest' :: SObjectName -> NonEmpty SObjectFieldName -> QueryStep [Object] a a
queryRest' obj_nm field_nms = do
  res <- runClient $ Rest.query $ buildQuery obj_nm field_nms
  allResults res
  where
    store' = store . RQueryResponse.records

    allResults :: Rest.QueryResponse -> QueryStep [Object] a a
    allResults resp = do
      r <- store' resp
      case RQueryResponse.nextRecordsUrl resp of
        Nothing -> return r
        Just next_url ->
          runClient (Rest.queryResults next_url) >>= store'


queryRestAllFields :: SObjectName -> QueryJob
queryRestAllFields obj_nm = QueryJob $ toQueryResults' $ queryRestAllFields' obj_nm

queryRestAllFields' :: SObjectName -> QueryStep [Object] a a
queryRestAllFields' obj_nm = do
  fields <- (NE.fromList . nonCompoundFields . NE.toList
              . SObjectDescribe.fields)
              <$> runClient (sobjectDescribe obj_nm)
  queryRest' obj_nm fields

runClient :: SfdcClient a -> QueryStep query_result_type b a
runClient c = do
  sess <- getSession
  QueryStep . liftIO $ SfdcClient.runClientWithSession sess c

queryJobResults :: JobId -- ^ Query job ID
                -> QueryStep ByteString a a
queryJobResults job_id = go Nothing
  where
    max_records = 5000
    go :: Maybe Text -> QueryStep ByteString a a
    go locator = do
      headers <- runClient $ Bulk.queryJobResults job_id locator (Just max_records)
      let resp = getResponse headers
      r <- store resp
      case lookupResponseHeader headers :: ResponseHeader "Sforce-Locator" Text of
        MissingHeader -> return r
        Header "null" -> return r
        Header next_locator -> go (Just next_locator)
        UndecodableHeader _ -> throwM $ OtherException "Undecodable header"