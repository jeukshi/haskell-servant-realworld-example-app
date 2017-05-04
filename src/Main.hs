{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad.Except             (liftIO)
import           Data.Aeson                       (Result (..), fromJSON,
                                                   toJSON)
import qualified Data.ByteString                  as BS
import qualified Data.Map                         as Map
import           Data.Proxy
import           Data.Text
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeUtf8)
import           Data.Time                        (UTCTime)
import           Database.SQLite.Simple           (Connection, open)
import           DB
import           Network.Wai
import           Network.Wai.Handler.Warp         (run)
import           Servant
import           Servant.API
import           Servant.Server.Experimental.Auth
import           Types
import qualified Web.JWT                          as JWT

-- FIXME Secret in source.
secret :: JWT.Secret
secret = JWT.secret "unsafePerformIO"

-- FIXME hardcoded database
connection :: IO Connection
connection = open "/tmp/haskell-servant-test.db"

main :: IO ()
main = do
  print "Let's go!"
  conn <- connection
  run 8081 (app conn)

app :: Connection -> Application
app conn = serveWithContext api serverAuthContext (server conn)
  where
    serverAuthContext
      :: Context
      (AuthHandler Request DBUser ': AuthHandler Request (Maybe DBUser) ': '[])
    serverAuthContext =
      (authHandler conn) :. (authOptionalHandler conn) :. EmptyContext

------------------------------------------------------------------------
-- | API

type API =
         -- | Authentication
           "api" :> "users" :> "login"
           :> ReqBody '[JSON] (User Login)
           :> Post '[JSON] (User AuthUser)
         -- | Registration
      :<|> "api" :> "users"
           :> ReqBody '[JSON] (User NewUser)
           :> Post '[JSON] (User (Maybe AuthUser))
         -- | Get Current User
      :<|> "api" :> "user"
           :> AuthProtect "JWT"
           :> Get '[JSON] (User AuthUser)
         -- | Update User
      :<|> "api" :> "user"
           :> AuthProtect "JWT" :> ReqBody '[JSON] (User UpdateUser)
           :> Put '[JSON] (User AuthUser)
         -- | Profile Get
      :<|> "api" :> "profiles" :> Capture "username" Username
           :> AuthProtect "JWT-optional"
           :> Get '[JSON] (Profile (Maybe UserProfile))
         -- | Follow
      :<|> "api" :> "profiles" :> Capture "username" Username :> "follow"
           :> AuthProtect "JWT"
           :> Post '[JSON] (Profile (Maybe UserProfile))
        -- | Unfollow
      :<|> "api" :> "profiles" :> Capture "username" Username :> "follow"
           :> AuthProtect "JWT"
           :> Delete '[JSON] (Profile (Maybe UserProfile))

api :: Proxy API
api = Proxy

------------------------------------------------------------------------
-- | Server

-- FIXME far from ideal managment of connection.
server :: Connection -> Server API
server conn = (loginHandler conn)
         :<|> (registerHandler conn)
         :<|> (getUserHandler conn)
         :<|> (updateUserHandler conn)
         :<|> (getProfileHandler conn)
         :<|> (followHandler conn)
         :<|> (unfollowHandler conn)

------------------------------------------------------------------------
-- | Auth

type instance AuthServerData (AuthProtect "JWT") = DBUser
type instance AuthServerData (AuthProtect "JWT-optional") = (Maybe DBUser)

-- TODO This is copy pasted authHandler returning Nothing instead of error.
-- Refactor common stuff.
authOptionalHandler :: Connection -> AuthHandler Request (Maybe DBUser)
authOptionalHandler conn =
  let handler req =
        case lookup "Authorization" (requestHeaders req) of
          Nothing -> return Nothing
          Just token ->
            case decodeToken token of
              Nothing -> return Nothing
              Just username -> do
                mbUser <- liftIO $ dbGetUserByName conn username
                return mbUser
  in mkAuthHandler handler

authHandler :: Connection -> AuthHandler Request DBUser
  -- FIXME Too nested.
authHandler conn =
  let handler req =
        case lookup "Authorization" (requestHeaders req) of
          Nothing ->
            throwError (err401 {errBody = "Missing 'Authorization' header"})
          Just token ->
            case decodeToken token of
              Nothing ->
                throwError (err401 {errBody = "Wrong 'Authorization' token"})
              Just username -> do
                usr <- liftIO $ dbGetUserByName conn username
                case usr of
                  Nothing ->
                    throwError (err401 {errBody = "User doesn't exist"})
                  Just usr -> return usr
  in mkAuthHandler handler

decodeToken :: BS.ByteString -> Maybe Username
decodeToken auth =
  -- TODO Maybe we should use safe version of decodeUtf8.
  let auth' = decodeUtf8 auth
      (_, token) = T.splitAt (T.length "Token ") auth'
      jwt = JWT.decodeAndVerifySignature secret token
      json =
        Map.lookup "username" =<<
        fmap (JWT.unregisteredClaims . JWT.claims) jwt
      username = fmap fromJSON json
  in case username of
        Just (Success (User x)) -> Just x
        _                       -> Nothing

token :: Username -> JWT.JSON
token username =
  JWT.encodeSigned
    JWT.HS256
    secret
    JWT.def
    {JWT.unregisteredClaims = Map.singleton "username" . toJSON $ User username}

------------------------------------------------------------------------
-- | Handlers

loginHandler :: Connection -> (User Login) -> Handler (User AuthUser)
loginHandler conn (User login) = do
  mbUser <- liftIO $ dbGetUserByLogin conn login
  case mbUser of
    Nothing -> throwError (err401 {errBody = "Incorrect login or password"})
    Just usr -> return $ User $ userToAuthUser usr

  -- TODO We should return error instead of Maybe.
registerHandler :: Connection -> (User NewUser) -> Handler (User (Maybe AuthUser))
registerHandler conn (User newUser) = do
  liftIO $ print "register handler"
  liftIO $ print newUser
  addedUser <- liftIO $ dbRegister conn newUser
  liftIO $ print addedUser
  return $ User $ fmap userToAuthUser addedUser

getUserHandler :: Connection -> DBUser -> Handler (User AuthUser)
getUserHandler conn usr = return $ User $ userToAuthUser usr

updateUserHandler :: Connection -> DBUser -> (User UpdateUser) -> Handler (User AuthUser)
updateUserHandler conn user (User update) = do
  let updatedUser = updateUser user update
  result <- liftIO $ dbUpdateUser conn updatedUser
  case result of
    Nothing -> throwError (err409 {errBody = "Username or email already taken"})
    Just x -> return $ User $ userToAuthUser x

getProfileHandler :: Connection -> Username -> Maybe DBUser -> Handler (Profile (Maybe UserProfile))
getProfileHandler conn name mbRequestee = do
  -- TODO follows
  liftIO $ print mbRequestee
  user <- liftIO $ dbGetUserByName conn name
  let profile = fmap (userToProfile False) user
  return $ Profile $ profile

followHandler :: Connection -> Username -> DBUser -> Handler (Profile (Maybe UserProfile))
followHandler conn name user = do
  toFollow <- liftIO $ dbGetUserByName conn name
  case toFollow of
    Nothing -> return $ Profile $ Nothing
    Just followed -> do
      liftIO $ dbFollow conn user followed
      return $ Profile $ Just $ userToProfile True followed

unfollowHandler :: Connection -> Username -> DBUser -> Handler (Profile (Maybe UserProfile))
unfollowHandler conn name user = do
  toUnfollow <- liftIO $ dbGetUserByName conn name
  case toUnfollow of
    Nothing -> return $ Profile $ Nothing
    Just unfollowed -> do
      liftIO $ dbUnfollow conn user unfollowed
      return $ Profile $ Just $ userToProfile False unfollowed

------------------------------------------------------------------------
-- | Utils

userToAuthUser :: DBUser -> AuthUser
userToAuthUser DBUser {..} =
  let aurEmail = usrEmail
      aurToken = token usrUsername
      aurUsername = usrUsername
      aurBio = usrBio
      aurImage = usrImage
  in AuthUser {..}
