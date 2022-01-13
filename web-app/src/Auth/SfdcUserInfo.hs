{-# LANGUAGE OverloadedStrings #-}

module Auth.SfdcUserInfo
  (UserInfo (..))
where

import Data.Text
import Data.Aeson

data UserInfo = UserInfo
  { userId :: Text
  , preferredUsername :: Text
  , email :: Text
  , emailVerified :: Bool
  }

instance FromJSON UserInfo where
  parseJSON = withObject "UserInfo" $ \o -> UserInfo
    <$> o .: "user_id"
    <*> o .: "preferred_username"
    <*> o .: "email"
    <*> o .: "email_verified"