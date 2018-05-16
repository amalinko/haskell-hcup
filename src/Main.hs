{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson

import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data User = User
  { id :: Int
  , email :: String
  , firstName :: String
  , lastName :: String
  , gender :: String
  , birthDate :: String
  } deriving (Eq, Show, Generic)

instance FromJSON User

instance ToJSON User

data Location = Location
  { lId :: Int
  , place :: String
  , country :: String
  , city :: String
  , distance :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON Location

instance ToJSON Location

type UserApi = "users" :> Capture "id" Int :> Get '[ JSON] User

type LocationApi = "locations" :> Get '[ JSON] Location

type API = UserApi :<|> LocationApi

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = users :<|> locations
  where
    users :: Int -> Handler User
    users id = return $ User 1 "example@mail.ru" "John" "Doe" "m" "1613433600"
    locations :: Handler Location
    locations = return $ Location 2 "Grand Canyon" "USA" "Las-Vegas" 100

main = startApp
