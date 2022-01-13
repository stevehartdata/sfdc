{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sfdc.Api.Bulk2_0.Ingest.TestData
  ( infiniteTestData
  , testData
  )
where

import Data.Csv
import Data.Text
import GHC.Generics

data Account = Account
  { name :: Text
  , numberOfEmployees :: Integer
  , phone :: Maybe Text
  }
  deriving (Generic, Show)

instance ToNamedRecord Account

instance DefaultOrdered Account

testData :: [Account]
testData =
  [ Account "Test1" 0 (Just "1-800-888-8910x7")
  , Account "Test2" 1 Nothing
  , Account "Test3" 2 (Just "2-800-889-9910")
  ]

infiniteTestData :: [Account]
infiniteTestData =
  fmap (\i -> Account ("Test" <> tshow i) i (Just "1-800-888-8888")) [1..]

tshow :: Show a => a -> Text
tshow = pack . show