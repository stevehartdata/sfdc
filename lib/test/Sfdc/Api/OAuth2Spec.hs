module Sfdc.Api.OAuth2Spec
  ( getAccessTokenWithConfig
  , spec
  ) where

import Test.Hspec
import Test.Hspec.Expectations

import Config (getConfig)
import qualified Config as C

import Control.Monad.Except (runExceptT)
import Data.Time.Clock (NominalDiffTime, getCurrentTime, addUTCTime)
import Data.Either (isRight, fromRight)

import Sfdc.Api.OAuth2

getAccessTokenWithConfig :: C.Config
                         -> NominalDiffTime -- ^ Number of seconds before token expires
                         -> IO TokenResponse
getAccessTokenWithConfig config seconds_to_exp = do
  token <- getAccessTokenWithConfig' config seconds_to_exp
  return $ either (\msg -> error $ "Couldn't get access token: " ++ msg) Prelude.id token


getAccessTokenWithConfig' :: C.Config
                          -> NominalDiffTime -- ^ Number of seconds before token expires
                          -> IO (Either String TokenResponse)
getAccessTokenWithConfig' config seconds_to_exp = do
  now <- getCurrentTime
  let getAccessToken' = getAccessToken
          (C.privateKey config)
          (C.clientId config)
          (C.authorizationServer config)
          (C.username config)
          (addUTCTime seconds_to_exp now)
  runExceptT getAccessToken'


spec :: Spec
spec = do
  config <- runIO getConfig

  it "can get an access token" $ do
    token <- getAccessTokenWithConfig' config 600
    (fmap (const "Redacted token") token)
      `shouldSatisfy` isRight
