{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( appMain
    , makeFoundation
    , makeLogWare
    ) where

import Control.Concurrent                   (forkIO)
import Control.Monad.Trans.Except (runExceptT, ExceptT (..), withExceptT)
import Control.Monad.Logger                 (liftLoc, runNoLoggingT,
                                             runLoggingT)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Database.Persist.Sqlite              (createSqlitePool, runSqlPool,
                                             sqlDatabase, sqlPoolSize)
import Import
import Language.Haskell.TH.Syntax           (qLocation)
import Network.HTTP.Client.TLS              (getGlobalManager)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             runSettings, setHost,
                                             setOnException, setPort, getPort)
import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                             IPAddrSource (..),
                                             OutputFormat (..), destination,
                                             mkRequestLogger, outputFormat)
import System.Log.FastLogger                (LoggerSet, defaultBufSize,
                                             newStdoutLoggerSet, toLogStr)

import Network.HTTP.Client hiding (queryString)
import Network.HTTP.Client.TLS

import Crypto.PubKey.RSA as RSA (PrivateKey)
import Data.X509 (PrivKey (..))
import Data.X509.File (readKeyFile)
import qualified Data.Set as Set
import Data.Time.Clock
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString.Lazy as BSL
import System.Directory

import Sfdc.Query
import Sfdc.SObjectName (SObjectName, appTabMember, colorDefinition,
  contentDocumentLink, contentFolderItem)
import qualified Sfdc.SObjectName as SObjectName
import Sfdc.AcceptedEventRelationField
import Sfdc.Api.Rest.Client (describeGlobal)
import Sfdc.Api.Rest.DescribeGlobalResponse (allQueryableObjects)
import Sfdc.Api.Session (AuthConfig (AuthConfig), Session, newSession)
import qualified Sfdc.Api.Session as AuthConfig (AuthConfig (..))
import Sfdc.Api.SfdcClient (runClientWithSession)
import Servant.Client
import Sfdc.Api.OAuth2 hiding (Exception)

import Queue
import qualified BackupJobStatus as BJS
import ObjectsToBackup

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Common
import Handler.BackupJob
import Handler.QueuedBackupJob

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: LoggerSet -> AppSettings -> PrivateKey -> BackupQueue -> IO App
makeFoundation loggerSet appSettings privKey appBackupQueue = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- getGlobalManager
    appLogger <- makeYesodLogger loggerSet
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)
    let appNewSfdcSession username = newSession $ AuthConfig
          { AuthConfig.privateKey = privKey
          , AuthConfig.clientId = T.unpack $ appSfdcClientId appSettings
          , AuthConfig.authorizationServer = MainAuthorizationServer
          , AuthConfig.username = T.unpack username
          , AuthConfig.tokenLifetime = appSfdcTokenLifetime appSettings
          }

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createSqlitePool
        (sqlDatabase $ appDatabaseConf appSettings)
        (sqlPoolSize $ appDatabaseConf appSettings)

    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    -- Return the foundation
    return $ mkFoundation pool

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }


-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

priv_key_path :: String
priv_key_path = "config/private_key.pem"

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- require values at runtime
        []

        -- allow environment variables to override
        useEnv

    priv_keys <- readKeyFile priv_key_path
    let priv_key =
            case priv_keys of
                [] -> error "No private key found"
                [priv_key] ->
                    case priv_key of
                        PrivKeyRSA priv_key_rsa -> priv_key_rsa
                        _ -> error "Expected RSA private key"
                _ -> error "More than one private key found"

    logger_set <- newStdoutLoggerSet defaultBufSize

    backup_db_pool <- runNoLoggingT
        $ createSqlitePool
            (sqlDatabase $ appDatabaseConf settings)
            5

    backup_queue <- newBackupQueue backup_db_pool

    -- Generate the foundation from the settings
    foundation <- makeFoundation logger_set settings priv_key backup_queue

    _ <- forkIO $ backupWorker
                    (unpack $ appBackupDir settings)
                    priv_key
                    (unpack $ appSfdcClientId settings) 
                    backup_queue
                    (appConnPool foundation)

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app



backupWorker :: FilePath -> RSA.PrivateKey -> String -> BackupQueue -> ConnectionPool -> IO ()
backupWorker dir priv_key client_id queue pool = do
  forever $ do
    (qbj_id, qbj) <- dequeue queue
    flip withException (setFailed qbj_id) $ do
      sess <- newSession $ AuthConfig
                { privateKey = priv_key
                , clientId = client_id
                , authorizationServer = MainAuthorizationServer -- TODO: Support other authorization servers
                , username = unpack $ queuedBackupJobSfdcUsername qbj
                , tokenLifetime = 600
                }
      let qbj_dir = dir </> (show qbj_id)
      createDirectory qbj_dir
      backupObjects qbj_dir sess $ queuedBackupJobObjectsToBackup qbj
      return ()
    setSucceeded qbj_id
    return ()
  where
  setSucceeded :: QueuedBackupJobId -> IO ()
  setSucceeded qbj_id = do
    now <- getCurrentTime
    flip runSqlPool pool
      $ update qbj_id [QueuedBackupJobEndedAt =. Just now]
    return ()

  setFailed :: QueuedBackupJobId -> SomeException -> IO ()
  setFailed qbj_id e = do
    now <- getCurrentTime
    flip runSqlPool pool
      $ update qbj_id
          [ QueuedBackupJobError =. (Just $ T.pack $ displayException e)
          , QueuedBackupJobEndedAt =. Just now
          ]
    return ()

-- Objects that may be queryable but that are unsupported for other reasons
unsupportedObjects :: Set SObjectName
unsupportedObjects = Set.fromList
  [ appTabMember -- Query requires a filter (see #24)
  , colorDefinition -- Query requires a filter (see #24)
  , contentDocumentLink -- Query requires a filter (see #25)
  , contentFolderItem -- Query requires a filter (see #26)
  ]

backupObjects :: FilePath -> Session -> ObjectsToBackup -> IO ()
backupObjects dir sess AllObjects = do
  objs <- runClientWithSession sess (allQueryableObjects <$> describeGlobal)
  let supported_objs = filter (`Set.notMember` unsupportedObjects) objs 
  flip mapM_ supported_objs $ backupObject dir sess
backupObjects dir sess (TheseObjects objs) =
  flip mapM_ (Set.toList objs) $ backupObject dir sess

backupObject :: FilePath -> Session -> SObjectName -> IO ()
backupObject dir sess o = do
  let obj_dir = dir </> (T.unpack . SObjectName.toText $ o)
  createDirectory obj_dir
  queryToFilesystem sess obj_dir $ queryAllFields o
