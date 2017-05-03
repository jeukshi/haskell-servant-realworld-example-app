{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DB where

import           Data.Text              (Text)
import           Database.SQLite.Simple
import           GHC.Int                (Int64)
import           Types

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
      \ WHERE usr_email = ? \
        \ AND usr_password = ? \
      \ LIMIT 1"

dbAddUser :: Connection -> NewUser -> IO DBUser
dbAddUser conn newUser = do
  execute conn stmt newUser
  id_ <- lastInsertRowId conn
  -- FIXME There should be more elegant way.
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
