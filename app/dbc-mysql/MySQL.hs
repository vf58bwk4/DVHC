{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module MySQL where

import           Data.List
import           Data.Pool (createPool, withResource)
import qualified Data.Text as T
import           Database.MySQL.Base
import           System.IO
import qualified System.IO.Streams as Streams

main :: IO ()
main = do
  System.IO.hSetEncoding stdout utf8

  let connectInfo = defaultConnectInfo {
    ciHost      = "localhost",
    ciDatabase  = "northwind",
    ciUser      = "root",
    ciPassword  = "nataliaa"
    }

  pool <- createPool (connect connectInfo) close 1 10 10

  withResource pool $ \conn -> do
    (_, is) <- query_ conn "SELECT * FROM orders WHERE orderId = 10249"
    putMySqlStreamAsCsv is

  where
    putMySqlStreamAsCsv is = do
      Streams.read is >>= \case 
        Nothing -> return ()
        Just row -> do
          putStrLn . mySqlRowAsCsv $ row
          putMySqlStreamAsCsv is

    mySqlRowAsCsv = intercalate "," . map (showSQLValue)
      where
        showSQLValue (MySQLInt32 i)   = show i
        showSQLValue (MySQLText  t)   = T.unpack t
        showSQLValue (MySQLDate  d)   = show d
        showSQLValue (MySQLDecimal d) = show d
        showSQLValue (MySQLNull)      = "NULL"
        showSQLValue (x)              = show x

--------------------------------------------------------------------------------------------------------------------------------
-- Backup
-- 

-- lls <- Streams.toList is
-- let strippedVals = map mySqlRowAsCsv lls
-- >OR<
-- strippedVals <- map mySqlRowAsCsv <$> (Streams.toList is)
-- 
-- putStrLn $ unlines strippedVals

-- System.IO.hSetBinaryMode stdout True
-- System.IO.hGetEncoding stdout >>= \case 
--   Nothing -> do putStrLn "Binary"
--   Just te -> do putStrLn . show $ te
