{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad.Except     (liftIO)
import           Data.Proxy
import           Data.Text
import           Data.Time                (UTCTime)
import           Database.SQLite.Simple   (Connection, open)
import           DB
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.API
import           Types

-- FIXME hardcoded database
connection :: IO Connection
connection = open "/tmp/haskell-servant-test.db"

main :: IO ()
main = do
  print "Let's go!"
  conn <- connection
  run 8081 (app conn)

app :: Connection -> Application
app conn = serve api (server conn)

type API =
         -- | AuthUserentication
           "api" :> "users" :> "login" :> ReqBody '[JSON] (User Login) :> Post '[JSON] (User AuthUser)
         -- | Registration
      :<|> "api" :> "users" :> ReqBody '[JSON] (User NewUser) :> Post '[JSON] (User (Maybe AuthUser))
         -- | Get Current User
      -- TODO
         -- | Update User
      :<|> "api" :> "user" :> ReqBody '[JSON] (User UpdateUser) :> Put '[JSON] (User AuthUser)

api :: Proxy API
api = Proxy

-- FIXME far from ideal managment of connection.
server :: Connection -> Server API
server conn = (login conn)
         :<|> (register conn)
         :<|> (updateUser conn)

login :: Connection -> (User Login) -> Handler (User AuthUser)
login conn (User login) = do
  liftIO $ print "login handler"
  liftIO $ print login
  return $ User $ AuthUser (logEmail login) "token" "username" Nothing Nothing

updateUser :: Connection -> (User UpdateUser) -> Handler (User AuthUser)
updateUser conn (User user) = do
  liftIO $ print "updateUser handler"
  liftIO $ print user
  let email =
        case (uusEmail user) of
          Just x  -> x
          Nothing -> ""
  return $ User $ AuthUser email "token" "username" Nothing Nothing

  -- TODO We should return error instead of Maybe.
register :: Connection -> (User NewUser) -> Handler (User (Maybe AuthUser))
register conn (User newUser) = do
  liftIO $ print "register handler"
  liftIO $ print newUser
  addedUser <- liftIO $ dbRegister conn newUser
  liftIO $ print addedUser
  return $ User $ fmap userToAuthUser addedUser

userToAuthUser :: DBUser -> AuthUser
userToAuthUser DBUser {..} =
  let aurEmail = usrEmail
      -- TODO JWT
      aurToken = "TODO"
      aurUsername = usrUsername
      aurBio = usrBio
      aurImage = usrImage
  in AuthUser {..}
