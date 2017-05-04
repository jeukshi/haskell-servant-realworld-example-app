{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

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
import           Data.Maybe                       (fromMaybe, isNothing)
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

------------------------------------------------------------------------
-- | Common

type Username = Text

newtype Password = Password { unPassword :: Text }
  deriving (Eq, Show, FromJSON, FromField, ToField)

data User a = User a
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (User a) where
  toJSON (User a) = object ["user" .= a]

instance FromJSON a => FromJSON (User a) where
  parseJSON = withObject "user" $ \o -> do
    a <- o .: "user"
    return (User a)

data Profile a = Profile a
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Profile a) where
  toJSON (Profile a) = object ["profile" .= a]

instance FromJSON a => FromJSON (Profile a) where
  parseJSON = withObject "profile" $ \o -> do
    a <- o .: "profile"
    return (Profile a)

------------------------------------------------------------------------
-- | Response body

-- | This is User from spec.
data AuthUser = AuthUser
  { aurEmail    :: Text
  , aurToken    :: Text
  , aurUsername :: Username
  , aurBio      :: Maybe Text
  , aurImage    :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON AuthUser where
  toJSON = genericToJSON toJSONoptions

------------------------------------------------------------------------
-- | Request body

data Login = Login
  { logEmail    :: Text
  , logPassword :: Password
  } deriving (Eq, Show, Generic)

instance FromJSON Login where
  parseJSON = genericParseJSON toJSONoptions

  -- TODO can't be Maybe - we need something isomorfic to Maybe
  -- otherwise we cant set field to null
data UpdateUser = UpdateUser
  { uusEmail    :: Maybe Text
  , uusUsername :: Maybe Username
  , uusBio      :: Maybe Text
  , uusImage    :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON UpdateUser where
  parseJSON = genericParseJSON toJSONoptions

  -- | TODO add fields
data NewUser = NewUser
  { nusEmail    :: Text
  , nusUsername :: Username
  , nusPassword :: Password
  , nusBio      :: Maybe Text
  , nusImage    :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON NewUser where
  parseJSON = genericParseJSON toJSONoptions

instance ToRow NewUser where
  toRow (NewUser e u p b i) = toRow (e, u, p, b, i)

data UserProfile = UserProfile
  { proUsername  :: Username
  , proBio       :: Maybe Text
  , proImage     :: Maybe Text
  , proFollowing :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON UserProfile where
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
  , artAuthor         :: UserProfile
  } deriving (Eq, Show, Generic)

instance ToJSON Article where
  toJSON = genericToJSON toJSONoptions

------------------------------------------------------------------------
-- | Database

data DBUser = DBUser
  { usrId       :: Int64
  , usrEmail    :: Text
  , usrUsername :: Username
  , usrPassword :: Password
  , usrBio      :: Maybe Text
  , usrImage    :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromRow DBUser where
  fromRow = DBUser <$> field <*> field <*> field <*> field <*> field <*> field

data DBFollows = DBFollows
  { fwsUsrId :: Int64
  , fwsFollowedUsrId :: Int64
  } deriving (Eq, Show)

instance FromRow DBFollows where
  fromRow = DBFollows <$> field <*> field

-- FIXME This way we can't set bio and image to null.
updateUser :: DBUser -> UpdateUser -> DBUser
updateUser DBUser {..} UpdateUser {..} =
      DBUser usrId
             (fromMaybe usrEmail uusEmail)
             (fromMaybe usrUsername uusUsername)
             usrPassword
             (if isNothing uusBio then usrBio else uusBio)
             (if isNothing uusImage then usrImage else uusImage)

userToProfile :: Bool -> DBUser -> UserProfile
userToProfile follows DBUser {..} = UserProfile usrUsername usrBio usrImage follows
