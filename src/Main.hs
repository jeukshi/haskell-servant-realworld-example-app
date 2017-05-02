{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import Data.Proxy
import Data.Time (UTCTime)
import Servant.API
import Servant
import Control.Monad.Except
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Types

main :: IO ()
main = do
  putStrLn "hello world"
  run 8081 app

app :: Application
app = serve api server

type API = "api" :> "user" :> Get '[JSON] (User Auth)
         -- | Authentication
      :<|> "api" :> "users" :> "login" :> ReqBody '[JSON] (User Login) :> Post '[JSON] (User Auth)
         -- | Registration
      :<|> "api" :> "users" :> ReqBody '[JSON] (User Reg) :> Post '[JSON] (User Auth)
         -- | Get Current User
      -- TODO
         -- | Update User
      :<|> "api" :> "user" :> ReqBody '[JSON] (User UserUpdate) :> Put '[JSON] (User Auth)

api :: Proxy API
api = Proxy

server :: Server API
server = auth
    :<|> login
    :<|> register
    :<|> updateUser

  where
    auth = return $ User $ Auth "mail@mail" "token" "username" "bio" Nothing
    -- login (User a) = return $ User $ Auth (logEmail a) "token" "username" "bio" Nothing
    login (User login) = do
      liftIO $ print login
      return $ User $ Auth (logEmail login) "token" "username" "bio" Nothing
    register (User reg) = do
      liftIO $ print reg
      return $ User $ Auth (regEmail reg) "token" "username" "bio" Nothing
    updateUser (User user) = do
      liftIO $ print user
      let email = case (uusEmail user) of
            Just x -> x
            Nothing -> ""
      return $ User $ Auth email "token" "username" "bio" Nothing

