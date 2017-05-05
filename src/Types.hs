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
import qualified Data.Text                        as T
import           Data.Time                        (UTCTime)
import           Database.SQLite.Simple           (SQLData (..), field)
import           Database.SQLite.Simple.FromField (FromField, ResultError (..),
                                                   fromField, returnError)
import           Database.SQLite.Simple.FromRow   (FromRow, fromRow)
import           Database.SQLite.Simple.Internal  (Field (..))
import           Database.SQLite.Simple.Ok        (Ok (..))
import           Database.SQLite.Simple.ToField   (ToField)
import           Database.SQLite.Simple.ToRow     (ToRow, toRow)
import           GHC.Generics                     (Generic)
import           GHC.Int                          (Int64)

toJSONoptions = defaultOptions {
              fieldLabelModifier = headToLower . drop 3 }
  where
    headToLower x = toLower (head x) : tail x

------------------------------------------------------------------------
-- | Common

-- | QueryParams
type Limit = Int
type Offset = Int
type Tagged = Text
type Author = Username
type FavoritedBy = Username

type Username = Text

newtype Tags = Tags { unTags :: [Text]}
  deriving (Eq, Show, FromJSON, ToJSON)

instance FromField Tags where
  fromField (Field (SQLText txt) _) = Ok $ Tags $ T.splitOn ";" txt
  fromField f                       = returnError ConversionFailed f "need a text"

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

data Art a = Art a
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Art a) where
  toJSON (Art a) = object ["article" .= a]

instance FromJSON a => FromJSON (Art a) where
  parseJSON = withObject "article" $ \o -> do
    a <- o .: "article"
    return (Art a)

data Arts a = Arts a Int
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Arts a) where
  toJSON (Arts a i) = object ["articles" .= a, "articlesCount" .= i]

data TagList a = TagList a
  deriving (Eq, Show)

instance ToJSON a => ToJSON (TagList a) where
  toJSON (TagList a) = object ["tags" .= a]

data Cmt a = Cmt a
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Cmt a) where
  toJSON (Cmt a) = object ["comment" .= a]

instance FromJSON a => FromJSON (Cmt a) where
  parseJSON = withObject "comment" $ \o -> do
    a <- o .: "comment"
    return (Cmt a)

data Cmts a = Cmts a
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Cmts a) where
  toJSON (Cmts a) = object ["comments" .= a]

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

data UserProfile = UserProfile
  { proUsername  :: Username
  , proBio       :: Maybe Text
  , proImage     :: Maybe Text
  , proFollowing :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON UserProfile where
  toJSON = genericToJSON toJSONoptions

instance FromRow UserProfile where
  fromRow = UserProfile <$> field <*> field <*> field <*> field

data Article = Article
  { artSlug           :: Text
  , artTitle          :: Text
  , artDescription    :: Text
  , artBody           :: Text
  , artCreatedAt      :: UTCTime
  , artUpdatedAt      :: Maybe UTCTime
  , artFavorited      :: Int
  , artFavoritesCount :: Int
  , artTagList        :: Maybe Tags
  , artAuthor         :: UserProfile
  } deriving (Eq, Show, Generic)

instance ToJSON Article where
  toJSON = genericToJSON toJSONoptions

instance FromRow Article where
  fromRow = Article <$> field <*> field <*> field <*> field <*> field <*> field <*>
    field <*> field <*> field <*> (UserProfile <$> field <*> field <*> field <*> field)

data Comment = Comment
  { cmtId           :: Int64
  , cmtBody           :: Text
  , cmtCreatedAt      :: UTCTime
  , cmtUpdatedAt      :: Maybe UTCTime
  , cmtAuthor         :: UserProfile
  } deriving (Eq, Show, Generic)

instance ToJSON Comment where
  toJSON = genericToJSON toJSONoptions

instance FromRow Comment where
  fromRow = Comment <$> field <*> field <*> field <*> field <*>
    (UserProfile <$> field <*> field <*> field <*> field)

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

data NewArticle = NewArticle
  { nrtTitle       :: Text
  , nrtDescription :: Text
  , nrtBody        :: Text
  , nrtTagList     :: Maybe Tags
  } deriving (Eq, Show, Generic)

instance FromJSON NewArticle where
  parseJSON = genericParseJSON toJSONoptions

-- TODO again we can't update field to null.
data UpdateArticle = UpdateArticle
  { urtTitle       :: Maybe Text
  , urtDescription :: Maybe Text
  , urtBody        :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON UpdateArticle where
  parseJSON = genericParseJSON toJSONoptions

data NewComment = NewComment { nmtBody :: Text }
  deriving (Eq, Show, Generic)

instance FromJSON NewComment where
  parseJSON = genericParseJSON toJSONoptions
------------------------------------------------------------------------
-- | Database

data Tag = Tag
  { tagId   :: Int64
  , tagText :: Text
  } deriving (Eq, Show)

instance FromRow Tag where
  fromRow = Tag <$> field <*> field

data DBArticle = DBArticle
  { drtId          :: Int64
  , drtSlug        :: Text
  , drtTitle       :: Text
  , drtDescription :: Text
  , drtBody        :: Text
  , drtCreatedAt   :: UTCTime
  , drtUpdatedAt   :: Maybe UTCTime
  , drtAuthor      :: Int64
  } deriving (Eq, Show, Generic)

instance ToJSON DBArticle where
  toJSON = genericToJSON toJSONoptions

instance FromRow DBArticle where
  fromRow = DBArticle <$> field <*> field <*> field <*> field
    <*> field <*> field <*> field <*> field

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

updateArticle :: DBArticle -> UpdateArticle -> DBArticle
updateArticle DBArticle {..} UpdateArticle {..} =
      DBArticle drtId
                -- TODO update slug if title changes
                drtSlug
                (fromMaybe drtTitle urtTitle)
                (fromMaybe drtDescription urtDescription)
                (fromMaybe drtBody urtBody)
                drtCreatedAt
                drtUpdatedAt
                drtAuthor
