{-# LANGUAGE TypeOperators, DataKinds, DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Sfdc.Api.OAuth2.LowLevel
  ( AccessToken (..)
  , TokenRequest (..)
  , TokenResponse (..)
  , token
  )
where

import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import GHC.Generics
import Servant.API
import Servant.Client
import Text.Casing
import Web.FormUrlEncoded ( ToForm (..))


type OAuth2_0Api
  = "services" :> "oauth2" :> "token"
    :> ReqBody '[FormUrlEncoded] TokenRequest :> Post '[JSON] TokenResponse

newtype AccessToken = AccessToken ShortText
  deriving (Eq, Ord, Show)

instance FromJSON AccessToken where
  parseJSON = fmap (AccessToken . ST.fromString) . parseJSON

data TokenRequest = TokenRequest
  { grantType :: String
  , assertion :: Text
  } deriving (Generic)

instance ToForm TokenRequest where
  toForm tr =
    [ ("grant_type", toQueryParam (grantType tr))
    , ("assertion", toQueryParam (assertion tr))]

data TokenResponse = TokenResponse
  { accessToken :: AccessToken
  , tokenType :: TokenType
  , scope :: Scope
  , idToken :: Maybe String
  , instanceUrl :: Url
  , id :: String
  , sfdcCommunityUrl :: Maybe Url
  , sfdcCommunityId :: Maybe String
  } deriving (Generic)

instance FromJSON TokenResponse where
  parseJSON =
    genericParseJSON
    $ defaultOptions { fieldLabelModifier = toQuietSnake . fromHumps }

type TokenType = String
type Scope = String
type Url = String

oauthApi :: Proxy OAuth2_0Api
oauthApi = Proxy

token :: TokenRequest -> ClientM TokenResponse
token = client oauthApi
