{-# LANGUAGE LambdaCase        #-}

module Odbc where

import qualified Data.ByteString.Char8 as C8
import           Data.List
import           Database.HDBC
import           Database.HDBC.ODBC
import           System.IO

main :: IO () 
main = do
  System.IO.hSetEncoding stdout utf8

  conn <- connectODBC "DSN=MySQL: northwind"
  stmt <- prepare conn "SELECT * FROM orders"
  _ <- execute stmt []
  putMySqlStreamAsCSV stmt
  disconnect conn

  where
    putMySqlStreamAsCSV stmt = do
      fetchRow stmt >>= \case
        Nothing -> return ()
        Just row -> do
          putStrLn . convertRow $ row
          putMySqlStreamAsCSV stmt

    convertRow = intercalate "," . map (showSQLValue)
      where
        showSQLValue (SqlInt32 i)       = show i
        showSQLValue (SqlByteString bs) = C8.unpack $ bs
        showSQLValue (SqlLocalDate ld)  = show ld
        showSQLValue (SqlDouble d)      = show d
        showSQLValue (SqlNull)          = "NULL"
        showSQLValue (x)                = show x

  -- rows <- quickQuery conn "SELECT * FROM categories" []
  -- let strippedVals = map convertRow rows
  -- print strippedVals
