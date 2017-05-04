{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DB where

import           Data.Maybe             (listToMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Database.SQLite.Simple
import           GHC.Int                (Int64)
import           Types

-- | Ignore query results.
newtype Ignore = Ignore Text

instance FromRow Ignore where
  fromRow = Ignore <$> field

dbRegister :: Connection -> NewUser -> IO (Maybe DBUser)
-- TODO We need transaction here.
dbRegister conn newUser = do
  existingUsers <-
    dbGetUsersByNameOrEmail conn (nusUsername newUser) (nusEmail newUser)
  case null existingUsers of
    False -> return Nothing
    True -> do
      addedUser <- dbAddUser conn newUser
      return $ Just addedUser

dbGetUsersByNameOrEmail :: Connection -> Text -> Text -> IO [DBUser]
dbGetUsersByNameOrEmail conn name email = do
  results <- query conn stmt [name, email] :: IO [DBUser]
  return results
  where
    stmt =
      "SELECT usr_id \
          \ , usr_email \
          \ , usr_username \
          \ , usr_password \
          \ , usr_bio \
          \ , usr_image \
       \ FROM users \
      \ WHERE usr_username = ? \
         \ OR usr_email = ? "

dbGetUserByName :: Connection -> Text -> IO (Maybe DBUser)
dbGetUserByName conn name = do
  results <- query conn stmt [name] :: IO [DBUser]
  case null results of
    True  -> return Nothing
    False -> return $ Just $ head results
  where
    stmt =
      "SELECT usr_id \
          \ , usr_email \
          \ , usr_username \
          \ , usr_password \
          \ , usr_bio \
          \ , usr_image \
       \ FROM users \
      \ WHERE usr_username = ? \
      \ LIMIT 1"

dbGetUserByLogin :: Connection -> Login -> IO (Maybe DBUser)
dbGetUserByLogin conn (Login email password) = do
  results <- query conn stmt [email, unPassword password] :: IO [DBUser]
  return $ listToMaybe results
  where
    stmt =
      "SELECT usr_id \
          \ , usr_email \
          \ , usr_username \
          \ , usr_password \
          \ , usr_bio \
          \ , usr_image \
       \ FROM users \
      \ WHERE usr_email = ? \
        \ AND usr_password = ? \
      \ LIMIT 1"

dbAddUser :: Connection -> NewUser -> IO DBUser
dbAddUser conn newUser = do
  execute conn stmt newUser
  id_ <- lastInsertRowId conn
  return $ newUserWithId newUser id_
  where
    stmt =
      "INSERT INTO users \
          \ ( usr_email \
          \ , usr_username \
          \ , usr_password \
          \ , usr_bio \
          \ , usr_image) \
     \ VALUES \
          \ (?, ?, ?, ?, ?) "

-- FIXME Remove this hack - XRecordWildCards shadowed record accesor.
userToId :: DBUser -> Int64
userToId = usrId

dbUpdateUser :: Connection -> DBUser -> IO (Maybe DBUser)
dbUpdateUser conn DBUser {..} = do
  -- Check if name or email are already taken.
  existingUsers <-
    dbGetUsersByNameOrEmail conn usrUsername usrEmail
  let existingUsers' = filter (\x -> (userToId x) /= usrId) existingUsers
  case null existingUsers' of
    False -> return Nothing
    True -> do
      _ <- execute conn stmt args
      return $ Just DBUser {..}
  where
    args =
        toRow
          ( usrEmail
          , usrUsername
          , unPassword usrPassword
          , usrBio
          , usrImage
          , usrId)
    stmt =
      "UPDATE users \
        \ SET usr_email = ? \
          \ , usr_username = ? \
          \ , usr_password = ? \
          \ , usr_bio = ? \
          \ , usr_image = ? \
      \ WHERE \
          \   usr_id = ?"


newUserWithId :: NewUser -> Int64 -> DBUser
newUserWithId NewUser {..} id_ =
  let usrId = id_
      usrEmail = nusEmail
      usrPassword = nusPassword
      usrUsername = nusUsername
      usrBio = nusBio
      usrImage = nusImage
  in DBUser {..}


dbIsUserFollowing :: Connection -> DBUser -> DBUser -> IO Bool
dbIsUserFollowing conn user followed = do
  results <- query conn stmt args :: IO [Ignore]
  return $ (not . null) results
  where
    args = [usrId user, usrId followed]
    stmt =
      " select \"\" \
        \ from follows \
       \ where fws_usr_id = ? \
         \ AND fws_follows_usr_id = ? "

dbIsArticleTagged :: Connection -> Int64 -> Tag -> IO Bool
dbIsArticleTagged conn art_id tag = do
  results <- query conn stmt args :: IO [Ignore]
  return $ (not . null) results
  where
    args = [art_id, tagId tag]
    stmt =
      " select \"\" \
        \ from tagged \
       \ where tgd_art_id = ? \
         \ AND tgd_tag_id = ? "

dbFollow :: Connection -> DBUser -> DBUser -> IO ()
dbFollow conn user toFollow = do
  -- | Do nothing if we already follow that user.
  isFollowed <- dbIsUserFollowing conn user toFollow
  case isFollowed of
    True  -> return ()
    False -> execute conn stmt args
  where
    args = toRow
          ( usrId user
          , usrId toFollow)
    stmt =
     "INSERT INTO follows \
          \ ( fws_usr_id \
          \ , fws_follows_usr_id) \
     \ VALUES \
          \ (?, ?) "

dbUnfollow :: Connection -> DBUser -> DBUser -> IO ()
dbUnfollow conn user toUnfollow = do
  -- | Do nothing if we don't follow that user.
  isFollowed <- dbIsUserFollowing conn user toUnfollow
  case isFollowed of
    False -> return ()
    True  -> execute conn stmt args
  where
    args =
        toRow
          ( usrId user
          , usrId toUnfollow)
    stmt =
     "DELETE FROM follows \
     \ WHERE fws_usr_id = ? \
       \ AND fws_follows_usr_id = ?"

dbGetTagByText :: Connection -> Text -> IO (Maybe Tag)
dbGetTagByText conn text = do
  results <- query conn stmt args :: IO [Tag]
  return $ listToMaybe results
  where
    args = [text]
    stmt =
      " SELECT tag_id \
           \ , tag_text \
        \ FROM tags \
       \ WHERE tag_text = ?"


dbAddTag :: Connection -> Text -> IO Tag
dbAddTag conn text = do
  mbTag <- dbGetTagByText conn text
  case mbTag of
    Just x -> return x
    Nothing -> do
      execute conn stmt args
      tagId <- lastInsertRowId conn
      return $ Tag tagId text
  where
    args = toRow $ Only text
    stmt =
     "INSERT INTO tags \
         \ (tag_text) \
    \ VALUES \
          \ (?) "

dbTagArticle :: Connection -> Int64 -> Tag -> IO ()
dbTagArticle conn art_id tag = do
  isTagged <- dbIsArticleTagged conn art_id tag
  case isTagged of
    True  -> return ()
    False -> execute conn stmt args
  where
    args = toRow
          ( art_id
          , tagId tag)
    stmt =
      "INSERT INTO tagged \
          \ ( tgd_art_id \
          \ , tgd_tag_id) \
     \ VALUES \
          \ (?, ?) "

-- TODO Move it somewhere else.
titleToSlug :: Text -> Text
titleToSlug = T.intercalate "-" . T.words

-- TODO check is slug exists and try different one.
dbAddArticle :: Connection -> DBUser -> NewArticle -> IO (Maybe Article)
dbAddArticle conn user article = do
  execute conn stmt args
  art_id <- lastInsertRowId conn
  let mbTags = fmap unTags $ nrtTagList article
  -- | Add tags to database and tag article.
  dbTags <- traverse (traverse (dbAddTag conn)) $ mbTags
  _ <- traverse (traverse (dbTagArticle conn art_id)) $ dbTags
  addedArticle <- dbGetArticleById conn user art_id
  return addedArticle
  where
    args = toRow
           ( titleToSlug $ nrtTitle article
           , nrtTitle article
           , nrtDescription article
           , nrtBody article
           , usrId user)
    stmt =
      "INSERT INTO articles \
          \ ( art_slug \
          \ , art_title \
          \ , art_description \
          \ , art_body \
          \ , art_usr_id) \
     \ VALUES \
          \ (?, ?, ?, ?, ?) "

-- TODO Add favourited and following.
dbGetArticleById :: Connection -> DBUser -> Int64 -> IO (Maybe Article)
dbGetArticleById conn user art_id = do
  results <- query conn stmt args :: IO [Article]
  return $ listToMaybe results
  where
    args = toRow (art_id, art_id)
    stmt =
      "SELECT art_slug \
          \ , art_title \
          \ , art_description \
          \ , art_body \
          \ , art_createdAt \
          \ , art_updatedAt \
          \ , 0 as favourided \
          \ , ifnull((select count(1) \
               \ from favourited \
              \ where fav_art_id = art_id \
              \ group by fav_art_id), 0) \
         \ AS favourites_count \
          \ , (select group_concat (tag_text, \";\") \
               \ from tagged \
               \ join tags on tag_id = tgd_tag_id \
              \ where tgd_art_id = art_id \
              \ group by tgd_art_id) \
          \ , author.usr_username \
          \ , author.usr_bio \
          \ , author.usr_image \
          \ , 0 as following \
       \ FROM articles \
       \ JOIN users author \
         \ ON art_usr_id=author.usr_id \
      \ WHERE art_id = ? and art_id = ? "

-- TODO Add favourited and following.
-- TODO copy pasted
dbGetArticleBySlug :: Connection -> Text -> IO (Maybe Article)
dbGetArticleBySlug conn slug = do
  results <- query conn stmt args :: IO [Article]
  return $ listToMaybe results
  where
    args = toRow (slug, slug)
    stmt =
      "SELECT art_slug \
          \ , art_title \
          \ , art_description \
          \ , art_body \
          \ , art_createdAt \
          \ , art_updatedAt \
          \ , 0 as favourided \
          \ , ifnull((select count(1) \
               \ from favourited \
              \ where fav_art_id = art_id \
              \ group by fav_art_id), 0) \
         \ AS favourites_count \
          \ , (select group_concat (tag_text, \";\") \
               \ from tagged \
               \ join tags on tag_id = tgd_tag_id \
              \ where tgd_art_id = art_id \
              \ group by tgd_art_id) \
          \ , author.usr_username \
          \ , author.usr_bio \
          \ , author.usr_image \
          \ , 0 as following \
       \ FROM articles \
       \ JOIN users author \
         \ ON art_usr_id=author.usr_id \
      \ WHERE art_slug = ? and art_slug = ? "


-- TODO update slug with title
dbGetDBArticleBySlug :: Connection -> DBUser -> Text -> IO (Maybe DBArticle)
dbGetDBArticleBySlug conn user slug = do
  results <- query conn stmt args :: IO [DBArticle]
  return $ listToMaybe results
  where
    args = toRow (slug, usrId user)
    stmt =
      "SELECT art_id \
          \ , art_slug \
          \ , art_title \
          \ , art_description \
          \ , art_body \
          \ , art_createdAt \
          \ , art_updatedAt \
          \ , art_usr_id \
       \ FROM articles \
      \ WHERE art_slug = ? \
        \ AND art_usr_id = ? "

dbUpdateArticle :: Connection -> DBUser -> DBArticle -> IO (Maybe Article)
dbUpdateArticle conn user DBArticle {..} = do
  -- TODO generate new slug.
  _ <- execute conn stmt args
  updated <- dbGetArticleById conn user drtId
  return updated
  where
    args =
        toRow
          ( drtSlug
          , drtTitle
          , drtDescription
          , drtBody
          , drtId)
    stmt =
      "UPDATE articles \
        \ SET art_slug = ? \
      \     , art_title = ? \
          \ , art_description = ? \
          \ , art_body = ? \
      \ WHERE art_id = ?"

dbGetFeed :: Connection -> Limit -> Offset -> DBUser -> IO [Article]
dbGetFeed conn limit offset user = do
  results <- query conn stmt args :: IO [Article]
  return results
  where
    args = toRow (usrId user, limit, offset)
    stmt =
      "SELECT art_slug \
          \ , art_title \
          \ , art_description \
          \ , art_body \
          \ , art_createdAt \
          \ , art_updatedAt \
          \ , 0 as favourided \
          \ , ifnull((select count(1) \
               \ from favourited \
              \ where fav_art_id = art_id \
              \ group by fav_art_id), 0) \
         \ AS favourites_count \
          \ , (select group_concat (tag_text, \";\") \
               \ from tagged \
               \ join tags on tag_id = tgd_tag_id \
              \ where tgd_art_id = art_id \
              \ group by tgd_art_id) \
          \ , author.usr_username \
          \ , author.usr_bio \
          \ , author.usr_image \
          \ , 0 as following \
       \ FROM articles \
       \ JOIN users author \
         \ ON art_usr_id=author.usr_id \
       \ JOIN follows \
         \ ON art_usr_id = fws_follows_usr_id \
      \ WHERE fws_usr_id = ? \
      \ ORDER BY art_createdAt DESC \
      \ LIMIT ? OFFSET ? "

-- TODO atm we return most recent articles.
dbGetArticles :: Connection
              -> Limit
              -> Offset
              -> Maybe Author
              -> Maybe Tagged
              -> Maybe FavouritedBy
              -> Maybe DBUser
              -> IO [Article]
dbGetArticles conn limit offset _ _ _ _ = do
  results <- query conn stmt args :: IO [Article]
  return results
  where
    args = toRow (limit, offset)
    stmt =
      "SELECT art_slug \
          \ , art_title \
          \ , art_description \
          \ , art_body \
          \ , art_createdAt \
          \ , art_updatedAt \
          \ , 0 as favourided \
          \ , ifnull((select count(1) \
               \ from favourited \
              \ where fav_art_id = art_id \
              \ group by fav_art_id), 0) \
         \ AS favourites_count \
          \ , (select group_concat (tag_text, \";\") \
               \ from tagged \
               \ join tags on tag_id = tgd_tag_id \
              \ where tgd_art_id = art_id \
              \ group by tgd_art_id) \
          \ , author.usr_username \
          \ , author.usr_bio \
          \ , author.usr_image \
          \ , 0 as following \
       \ FROM articles \
       \ JOIN users author \
         \ ON art_usr_id=author.usr_id \
      \ ORDER BY art_createdAt DESC \
      \ LIMIT ? OFFSET ? "

dbDeleteArticle :: Connection -> DBArticle -> IO ()
dbDeleteArticle conn article = do
  let art_id = Only $ drtId article
  _ <- execute conn deleteFavourited art_id
  _ <- execute conn deleteTagged art_id
  _ <- execute conn deleteArticles art_id
  return ()
  where
    deleteFavourited = "DELETE FROM favourited where fav_art_id = ? "
    deleteTagged = "DELETE FROM tagged where tgd_art_id = ? "
    deleteArticles = "DELETE FROM articles where art_id = ? "

dbGetTags :: Connection -> IO [Tag]
dbGetTags conn = do
  results <- query conn stmt () :: IO [Tag]
  return results
  where
    stmt = "select tag_id, tag_text from tags"
