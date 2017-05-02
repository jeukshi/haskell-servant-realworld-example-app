{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Text (Text)
import Data.Aeson (decode)
import Data.Aeson.Types (withObject, parseJSON, genericParseJSON, FromJSON, ToJSON, toJSON, object, (.:), (.=), fieldLabelModifier, defaultOptions, genericToJSON)
import Data.Char (toLower)
import GHC.Generics (Generic)

toJSONoptions = defaultOptions {
             fieldLabelModifier = map toLower . drop 3 }

data User a = User a
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (User a) where
  toJSON (User a) = object ["user" .= a]

instance FromJSON a => FromJSON (User a) where
  parseJSON = withObject "user" $ \o -> do
    a <- o .: "user"
    return (User a)

data Login = Login
  { logEmail :: String
  , logPassword :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Login where
  toJSON = genericToJSON toJSONoptions

instance FromJSON Login where
  parseJSON = genericParseJSON toJSONoptions

data Auth = Auth
  { aurEmail :: String
  , aurToken :: String
  , aurUsername :: String
  , aurBio :: String
  , aurImage :: Maybe String
  } deriving (Eq, Show, Generic)

instance ToJSON Auth where
  toJSON = genericToJSON toJSONoptions

  -- TODO can't be Maybe - we need something isomorfic to Maybe
  -- otherwise we cant set field to null
data UserUpdate = UserUpdate
  { uusEmail :: Maybe String
  , uusToken :: Maybe String
  , uusUsername :: Maybe String
  , uusBio :: Maybe String
  , uusImage :: Maybe String
  } deriving (Eq, Show, Generic)

instance ToJSON UserUpdate where
  toJSON = genericToJSON toJSONoptions

instance FromJSON UserUpdate where
  parseJSON = genericParseJSON toJSONoptions

  -- | TODO add fields
data Reg = Reg
  { regEmail :: String
  , regUsername :: String
  , regPassword :: Maybe String
  } deriving (Eq, Show, Generic)

instance ToJSON Reg where
  toJSON = genericToJSON toJSONoptions

instance FromJSON Reg where
  parseJSON = genericParseJSON toJSONoptions

data Profile = Profile
  { proUsername :: String
  , proBio :: String
  , proImage :: String
  , proFollowing :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON Profile where
  toJSON = genericToJSON toJSONoptions

data Article = Article
  -- TODO types
  { artSlug :: String
  , artTitle :: String
  , artDescription :: String
  , artBody :: String
  , artCreatedAt :: String
  , artUpdatedAt :: String
  , artFavorited :: String
  , artFavoritesCount :: String
  , artAuthor :: Profile
  } deriving (Eq, Show, Generic)

instance ToJSON Article where
  toJSON = genericToJSON toJSONoptions
