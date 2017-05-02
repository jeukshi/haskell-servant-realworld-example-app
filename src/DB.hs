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
  user <-
    dbGetUserByNameOrEmail conn (nusUsername newUser) (nusEmail newUser)
  case user of
    Just _ -> return Nothing
    Nothing -> do
      u <- dbAddUser conn newUser
      return $ Just u

dbGetUserByNameOrEmail :: Connection -> Text -> Text -> IO (Maybe DBUser)
dbGetUserByNameOrEmail conn name email = do
  results <- query conn stmt [name, email] :: IO [DBUser]
  case null results of
    True -> return Nothing
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
         \ OR usr_email = ? \
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
          \ (?, ?, ?, ?, ?)"

newUserWithId :: NewUser -> Int64 -> DBUser
newUserWithId NewUser {..} id_ =
  let usrId = id_
      usrEmail = nusEmail
      usrPassword = nusPassword
      usrUsername = nusUsername
      usrBio = nusBio
      usrImage = nusImage
  in DBUser {..}
