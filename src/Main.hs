{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad.Except
import           Data.Proxy
import           Data.Text
import           Data.Time                (UTCTime)
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.API
import           Types

main :: IO ()
main = do
  putStrLn "hello world"
  run 8081 app

app :: Application
app = serve api server

type API =
         -- | Authentication
           "api" :> "users" :> "login" :> ReqBody '[JSON] (User Login) :> Post '[JSON] (User Auth)
         -- | Registration
      :<|> "api" :> "users" :> ReqBody '[JSON] (User Reg) :> Post '[JSON] (User Auth)
         -- | Get Current User
      -- TODO
         -- | Update User
      :<|> "api" :> "user" :> ReqBody '[JSON] (User UserUpdate) :> Put '[JSON] (User Auth)

api :: Proxy API
api = Proxy

server :: Server API
server = login
    :<|> register
    :<|> updateUser

login :: (User Login) -> Handler (User Auth)
login (User login) = do
  liftIO $ print login
  return $ User $ Auth (logEmail login) "token" "username" "bio" Nothing

updateUser :: (User UserUpdate) -> Handler (User Auth)
updateUser (User user) = do
  liftIO $ print user
  let email = case (uusEmail user) of
        Just x  -> x
        Nothing -> ""
  return $ User $ Auth email "token" "username" "bio" Nothing


register :: (User Reg) -> Handler (User Auth)
register (User reg) = do
      liftIO $ print reg
      return $ User $ Auth (regEmail reg) "token" "username" "bio" Nothing
