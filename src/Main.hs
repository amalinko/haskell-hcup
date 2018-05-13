{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BCL
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import GHC.Generics
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  counter <- newIORef 0
  putStrLn "http://localhost:8080/"
  run 8080 (app counter)

app :: IORef Int -> Application
app counter request respond =
  case pathInfo request of
    ["hello"] -> do
      current <-
        atomicModifyIORef'
          counter
          (\c ->
             let x = c + 1
             in (x, x))
      respond $
        responseLBS
          status200
          [("Content-Type", "text/plain")]
          (BCL.pack ("Hello! You are the " ++ (show current ++ " one")))
    ["users"] ->
      case parseMethod $ requestMethod request of
        (Right GET) -> do
          let user = User 1 "test@email.com"
              json = encode user
          respond $
            responseLBS status200 [("Content-Type", "application/json")] json
        (Right POST) -> do
          b <- requestBody request
          print b
          respond $
            responseLBS status200 [("Content-Type", "application/json")] "ssss"
    other ->
      respond $ responseLBS status200 [("Content-Type", "text/plain")] "11111"
  where
    make404 :: B.ByteString -> BL.ByteString
    make404 route = BCL.concat [BCL.pack "Not found ", BL.fromStrict route]

data User = User
  { id :: Int
  , email :: String
  } deriving (Show, Generic)

instance ToJSON User

instance FromJSON User
