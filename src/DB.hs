{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DB where

import           Data.List              (intercalate)
import           Data.Maybe             (catMaybes, fromMaybe, isJust,
                                         listToMaybe)
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

-- TODO Add favorited and following.
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
               \ from favorited \
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

-- TODO Add favorited and following.
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
               \ from favorited \
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
dbGetUsersDBArticleBySlug :: Connection -> DBUser -> Text -> IO (Maybe DBArticle)
dbGetUsersDBArticleBySlug conn user slug = do
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

dbGetDBArticleBySlug :: Connection -> Text -> IO (Maybe DBArticle)
dbGetDBArticleBySlug conn slug = do
  results <- query conn stmt args :: IO [DBArticle]
  return $ listToMaybe results
  where
    args = toRow (Only slug)
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
      \ WHERE art_slug = ? "


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
               \ from favorited \
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
              -> Maybe FavoritedBy
              -> Maybe DBUser
              -> IO [Article]
dbGetArticles conn limit offset mbAuthor mbTagged mbFavoritedBy mbUser = do
  results <- query conn stmt args :: IO [Article]
  return results
  where
    userId = fromMaybe 0 $ fmap usrId mbUser

    args = toRow $ whereArgs ++ [T.pack . show $ userId, T.pack . show $ limit, T.pack . show $ offset]
    mbWhereArgs = [mbAuthor, mbTagged] -- , mbFavoritedBy, fmap usrUsername mbUser]
    whereNames = ["author.usr_username = ?", "tag_text = ?"]
    whereArgs = catMaybes mbWhereArgs
    whereString = intercalate " AND "
                . map snd
                . filter (isJust . fst)
                $ zip mbWhereArgs whereNames
    -- TODO String -> Text
    stmt = Query $ T.pack stmt'
    stmt' =
      "SELECT art_slug \
          \ , art_title \
          \ , art_description \
          \ , art_body \
          \ , art_createdAt \
          \ , art_updatedAt \
          \ , 0 as favourided \
          \ , ifnull((select count(1) \
               \ from favorited \
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
          \ , CASE WHEN fws_usr_id is not null THEN 1 ELSE 0 END as following \
       \ FROM articles \
       \ JOIN users author \
         \ ON art_usr_id=author.usr_id \
       \ LEFT JOIN tagged \
         \ ON art_id= tgd_art_id \
       \ LEFT JOIN follows \
         \ ON author.usr_id=fws_follows_usr_id AND fws_usr_id = ? \
       \ LEFT JOIN tags \
         \ ON tgd_tag_id=tag_id "
         ++ (if null whereString then "" else " WHERE " ++ whereString) ++
      " GROUP BY art_id \
      \ ORDER BY art_createdAt DESC \
      \ LIMIT ? OFFSET ? "

dbDeleteArticle :: Connection -> DBArticle -> IO ()
dbDeleteArticle conn article = do
  let art_id = Only $ drtId article
  _ <- execute conn deleteFavorited art_id
  _ <- execute conn deleteTagged art_id
  _ <- execute conn deleteArticles art_id
  return ()
  where
    deleteFavorited = "DELETE FROM favorited where fav_art_id = ? "
    deleteTagged = "DELETE FROM tagged where tgd_art_id = ? "
    deleteArticles = "DELETE FROM articles where art_id = ? "

dbGetTags :: Connection -> IO [Tag]
dbGetTags conn = do
  results <- query conn stmt () :: IO [Tag]
  return results
  where
    stmt = "select tag_id, tag_text from tags"

dbIsFavoritedByUser :: Connection -> DBUser -> DBArticle -> IO Bool
dbIsFavoritedByUser conn user article = do
  results <- query conn stmt args :: IO [Ignore]
  return $ (not . null) results
  where
    args = [usrId user, drtId article]
    stmt =
      " select \"\" \
        \ from favorited \
       \ where fav_usr_id = ? \
         \ AND fav_art_id = ? "

dbFavoriteArticle :: Connection -> DBUser -> DBArticle -> IO ()
dbFavoriteArticle conn user article = do
  isFavorited <- dbIsFavoritedByUser conn user article
  case isFavorited of
    True  -> return ()
    False -> execute conn stmt args
  where
    args = toRow
          ( usrId user
          , drtId article)
    stmt =
     "INSERT INTO favorited \
          \ ( fav_usr_id \
          \ , fav_art_id) \
     \ VALUES \
          \ (?, ?) "

dbUnfavoriteArticle :: Connection -> DBUser -> DBArticle -> IO ()
dbUnfavoriteArticle conn user article = do
  _ <- execute conn stmt args
  return ()
  where
    args =
        toRow
          ( usrId user
          , drtId article)
    stmt =
     "DELETE FROM favorited \
     \ WHERE fav_usr_id = ? \
       \ AND fav_art_id = ?"

  -- TODO following
dbGetCommentById :: Connection -> Int64 -> IO (Maybe Comment)
dbGetCommentById conn cmt_id = do
  results <- query conn stmt args :: IO [Comment]
  return $ listToMaybe results
  where
    args = Only cmt_id
    stmt =
      "SELECT cmt_id \
          \ , cmt_body \
          \ , cmt_createdAt \
          \ , cmt_updatedAt \
          \ , author.usr_username \
          \ , author.usr_bio \
          \ , author.usr_image \
          \ , 0 as following \
       \ FROM comments \
       \ JOIN users author \
         \ ON cmt_usr_id=author.usr_id \
      \ WHERE cmt_id = ? "

dbAddComment :: Connection -> NewComment -> DBUser -> DBArticle -> IO (Maybe Comment)
dbAddComment conn newComment user article = do
  execute conn stmt args
  cmt_id <- lastInsertRowId conn
  comment <- dbGetCommentById conn cmt_id
  return comment
  where
    args = toRow
          ( nmtBody newComment
          , drtId article
          , usrId user)
    stmt =
     "INSERT INTO comments \
          \ ( cmt_body \
          \ , cmt_art_id \
          \ , cmt_usr_id) \
     \ VALUES \
          \ (?, ?, ?) "


dbDeleteComment :: Connection -> DBArticle -> DBUser -> Int -> IO ()
dbDeleteComment conn article user art_id = do
  _ <- execute conn stmt args
  return ()
  where
    args =
        toRow
          ( usrId user
          , drtId article
          , art_id)
    stmt =
     "DELETE FROM comments \
     \ WHERE cmt_usr_id = ? \
       \ AND cmt_art_id = ? \
       \ AND cmt_id = ?"

  -- TODO following
dbGetComments :: Connection -> DBArticle -> Maybe DBUser -> IO [Comment]
dbGetComments conn article mbUser = do
  results <- query conn stmt args :: IO [Comment]
  return results
  where
    args = Only $ drtId article
    stmt =
      "SELECT cmt_id \
          \ , cmt_body \
          \ , cmt_createdAt \
          \ , cmt_updatedAt \
          \ , author.usr_username \
          \ , author.usr_bio \
          \ , author.usr_image \
          \ , 0 as following \
       \ FROM comments \
       \ JOIN users author \
         \ ON cmt_usr_id=author.usr_id \
      \ WHERE cmt_art_id = ? "

