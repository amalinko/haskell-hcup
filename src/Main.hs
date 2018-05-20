{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson

import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
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

data Visit = Visit
  { vId :: Int
  , location :: Int
  , user :: Int
  , visitedAt :: Int
  , mark :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON Visit

instance ToJSON Visit

type UserApi = "users" :> Capture "id" Int :> Get '[ JSON] User

type LocationApi = "locations" :> Get '[ JSON] Location

type API = UserApi :<|> LocationApi

startApp :: IO ()
startApp = do
  let uMap = M.empty
      uMap' =
        M.insert 1 (User 1 "example@mail.ru" "John" "Doe" "m" "1613433600") uMap
  users <- newIORef uMap'
  a <- app users
  run 8080 a

app :: IORef (M.Map Int User) -> IO Application
app users = do
  print "servert started"
  return $ serve api (server users)

api :: Proxy API
api = Proxy

server :: IORef (M.Map Int User) -> Server API
server users = usersRoute :<|> locations
  where
    usersRoute :: Int -> Handler User
    usersRoute id = do
      u <- liftIO $ readIORef users
      case M.lookup id u of
        Just u' -> return u'
        Nothing -> throwError (ServantErr 404 "User not found" "" [])
    locations :: Handler Location
    locations = return $ Location 2 "Grand Canyon" "USA" "Las-Vegas" 100

main = startApp
