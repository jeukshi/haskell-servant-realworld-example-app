{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad.Except     (liftIO)
import           Data.Aeson               (Result (..), fromJSON, toJSON)
import qualified Data.Map                 as Map
import           Data.Proxy
import           Data.Text
import qualified Data.Text                as T
import           Data.Time                (UTCTime)
import           Database.SQLite.Simple   (Connection, open)
import           DB
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.API
import           Types
import qualified Web.JWT                  as JWT

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
app conn = serve api (server conn)

type API =
         -- | AuthUserentication
           "api" :> "users" :> "login" :> ReqBody '[JSON] (User Login) :> Post '[JSON] (User AuthUser)
         -- | Registration
      :<|> "api" :> "users" :> ReqBody '[JSON] (User NewUser) :> Post '[JSON] (User (Maybe AuthUser))
         -- | Get Current User
      :<|> "api" :> "user" :> Header "Authorization" Text :> Get '[JSON] (Maybe (User Username))
         -- | Update User
      :<|> "api" :> "user" :> ReqBody '[JSON] (User UpdateUser) :> Put '[JSON] (User AuthUser)

api :: Proxy API
api = Proxy

-- FIXME far from ideal managment of connection.
server :: Connection -> Server API
server conn = (login conn)
         :<|> (register conn)
         :<|> (getUser conn)
         :<|> (updateUser conn)

getUser :: Connection -> Maybe Text -> Handler (Maybe (User Username))
getUser conn auth =
  case auth of
    Nothing -> throwError err401
    Just x -> do
      let (_, token) = T.splitAt (T.length "Token ") x
          jwt = JWT.decodeAndVerifySignature secret token
          json =
            Map.lookup "username" =<<
            fmap (JWT.unregisteredClaims . JWT.claims) jwt
          username = fmap fromJSON json
      case username of
        Just (Success x) -> return $ Just x
        _                -> return $ Nothing

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
      aurToken = token usrUsername
      aurUsername = usrUsername
      aurBio = usrBio
      aurImage = usrImage
  in AuthUser {..}

token :: Username -> JWT.JSON
token username =
  JWT.encodeSigned
    JWT.HS256
    secret
    JWT.def
    {JWT.unregisteredClaims = Map.singleton "username" . toJSON $ User username}
