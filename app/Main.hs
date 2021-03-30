module Main where

-- {-# LANGUAGE OverloadedStrings #-}
import qualified Network.HTTP.Types as HTTP
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp (run)

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB8
-- import           Data.CaseInsensitive (mk)
-- import Lib

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    Warp.run 8080 (app3 "Hello, monkey!")

app3 :: String -> Application
app3 hello request respond = do
    putStrLn hello
    respond $ 
        case (B8.unpack . rawPathInfo) request of
            "/categories.csv" -> respondByCSV "db/categories.csv"
            "/customers.csv"  -> respondByCSV "db/customers.csv"
            _ -> notFound

respondByCSV :: FilePath -> Response
respondByCSV filePath = responseFile
    HTTP.status200
    [(HTTP.hContentType, B8.pack "text/csv")]
    filePath
    Nothing

notFound :: Response
notFound = responseLBS
    HTTP.status404
    [(HTTP.hContentType, B8.pack "text/plain")]
    (LB8.pack "404 - Not Found")

--
-- Backup
-- 
app2 :: Application
app2 _ respond = 
    respond $ responseFile
        HTTP.status200
        [(HTTP.hContentType, B8.pack "text/csv")]
        "categories.csv"
        Nothing

app :: String -> Application
app hello _ respond = do
    putStrLn hello
    respond $ responseLBS
        HTTP.status200
        [(HTTP.hContentType, B8.pack "text/plain")]
        (LB8.pack "Hello, Web")
