{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Types where

import           Data.Aeson                       (decode)
import           Data.Aeson.Types                 (FromJSON, ToJSON,
                                                   defaultOptions,
                                                   fieldLabelModifier,
                                                   genericParseJSON,
                                                   genericToJSON, object,
                                                   parseJSON, toJSON,
                                                   withObject, (.:), (.=))
import           Data.Char                        (toLower)
import           Data.Text                        (Text)
import           Database.SQLite.Simple           (field)
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.FromRow   (FromRow, fromRow)
import           Database.SQLite.Simple.ToField   (ToField)
import           Database.SQLite.Simple.ToRow     (ToRow, toRow)
import           GHC.Generics                     (Generic)
import           GHC.Int                          (Int64)

toJSONoptions = defaultOptions {
             fieldLabelModifier = map toLower . drop 3 }

newtype Password = Password Text
  deriving (Eq, Show, FromJSON, FromField, ToField)

data User a = User a
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (User a) where
  toJSON (User a) = object ["user" .= a]

instance FromJSON a => FromJSON (User a) where
  parseJSON = withObject "user" $ \o -> do
    a <- o .: "user"
    return (User a)

data Login = Login
  { logEmail    :: Text
  , logPassword :: Password
  } deriving (Eq, Show, Generic)

instance FromJSON Login where
  parseJSON = genericParseJSON toJSONoptions

data DBUser = DBUser
  { usrId       :: Int64
  , usrEmail    :: Text
  , usrUsername :: Text
  , usrPassword :: Password
  , usrBio      :: Maybe Text
  , usrImage    :: Maybe String
  } deriving (Eq, Show, Generic)

instance FromRow DBUser where
  fromRow = DBUser <$> field <*> field <*> field <*> field <*> field <*> field

-- | This is User from spec.
data AuthUser = AuthUser
  { aurEmail    :: Text
  , aurToken    :: Text
  , aurUsername :: Text
  , aurBio      :: Maybe Text
  , aurImage    :: Maybe String
  } deriving (Eq, Show, Generic)

instance ToJSON AuthUser where
  toJSON = genericToJSON toJSONoptions

  -- TODO can't be Maybe - we need something isomorfic to Maybe
  -- otherwise we cant set field to null
data UpdateUser = UpdateUser
  { uusEmail    :: Maybe Text
  , uusToken    :: Maybe Text
  , uusUsername :: Maybe Text
  , uusBio      :: Maybe Text
  , uusImage    :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON UpdateUser where
  parseJSON = genericParseJSON toJSONoptions

  -- | TODO add fields
data NewUser = NewUser
  { nusEmail    :: Text
  , nusUsername :: Text
  , nusPassword :: Password
  , nusBio      :: Maybe Text
  , nusImage    :: Maybe String
  } deriving (Eq, Show, Generic)

instance FromJSON NewUser where
  parseJSON = genericParseJSON toJSONoptions

instance ToRow NewUser where
  toRow (NewUser e u p b i) = toRow (e, u, p, b, i)

data Profile = Profile
  { proUsername  :: Text
  , proBio       :: Maybe Text
  , proImage     :: Text
  , proFollowing :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON Profile where
  toJSON = genericToJSON toJSONoptions

data Article = Article
  -- TODO types
  { artSlug           :: Text
  , artTitle          :: Text
  , artDescription    :: Text
  , artBody           :: Text
  , artCreatedAt      :: Text
  , artUpdatedAt      :: Text
  , artFavorited      :: Text
  , artFavoritesCount :: Text
  , artAuthor         :: Profile
  } deriving (Eq, Show, Generic)

instance ToJSON Article where
  toJSON = genericToJSON toJSONoptions
