{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BCL
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)

app :: Application
app request respond =
  respond $
  case (pathInfo request) of
    ["hello"] ->
      responseLBS status200 [("Content-Type", "text/plain")] "Hello!"
    other ->
      responseLBS status200 [("Content-Type", "text/plain")] "11111"
  where
    make404 :: B.ByteString -> BL.ByteString
    make404 route = BCL.concat [BCL.pack "Not found ", BL.fromStrict route]

main :: IO ()
main = do
  putStrLn $ "http://localhost:8080/"
  run 8080 app