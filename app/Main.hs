{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString.Builder
import           Data.List
import           Data.Pool
import qualified Data.Text as T
import           Database.MySQL.Base
import qualified Network.HTTP.Types as HTTP
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp (run)
import qualified System.IO.Streams as Streams

main :: IO ()
main = do
  let dbConnectInfo = defaultConnectInfo {
    ciHost      = "localhost",
    ciDatabase  = "northwind",
    ciUser      = "root",
    ciPassword  = "nataliaa"
    }

  dbPool <- createPool (connect dbConnectInfo) close 1 10 10

  putStrLn $ "http://localhost:8080/"
  Warp.run 8080 (app3 dbPool)

app3 :: Pool MySQLConn -> Request -> (Response -> t) -> t
app3 pool request respond = do
    respond $ 
        case rawPathInfo request of
            "/categories.csv" -> respondByFile "db/categories.csv"
            "/customers.csv"  -> respondByFile "db/customers.csv"
            "/orders.csv"     -> respondFromDb pool
            _ -> notFound

respondFromDb :: Pool MySQLConn -> Response
respondFromDb pool = responseStream 
  HTTP.status200 
  [(HTTP.hContentType, "text/csv")] 
  streamBody
  
  where
    streamBody write flush =
        withResource pool $ \conn -> do
          (_, is) <- query_ conn "SELECT * FROM orders"
          putMySqlStreamAsCsv is

        where
          putMySqlStreamAsCsv is = do
            Streams.read is >>= \case 
              Nothing -> return ()
              Just row -> do
                _ <- write . stringUtf8 $ mySqlRowAsCsv( row ) ++ "\n"
                _ <- flush
                putMySqlStreamAsCsv is

          mySqlRowAsCsv = intercalate "," . map (showSQLValue)
          showSQLValue (MySQLInt32 i)   = show i
          showSQLValue (MySQLText  t)   = T.unpack t
          showSQLValue (MySQLDate  d)   = show d
          showSQLValue (MySQLDecimal d) = show d
          showSQLValue (MySQLNull)      = "NULL"
          showSQLValue (x)              = show x        

respondByFile :: FilePath -> Response
respondByFile filePath = responseFile
    HTTP.status200
    [(HTTP.hContentType, "text/csv")]
    filePath
    Nothing

notFound :: Response
notFound = responseLBS
    HTTP.status404
    [(HTTP.hContentType, "text/plain")]
    "Not Found"

--------------------------------------------------------------------------------------------------------------------------------
-- Backup
-- 
app2 :: Application
app2 _ respond = 
    respond $ responseFile
        HTTP.status200
        [(HTTP.hContentType, "text/csv")]
        "categories.csv"
        Nothing

app :: String -> Application
app hello _ respond = do
    putStrLn hello
    respond $ responseLBS
        HTTP.status200
        [(HTTP.hContentType, "text/plain")]
        "Hello, Web"
