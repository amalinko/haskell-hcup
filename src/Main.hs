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

type API = "users" :> Get '[ JSON] User

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return $ User 1 "example@mail.ru" "John" "Doe" "m" "1613433600"

main = startApp
