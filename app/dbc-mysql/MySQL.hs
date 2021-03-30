{-# LANGUAGE OverloadedStrings #-}

module MySQL where

import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Data.List
import Data.List.Split

main = do
  conn <- connect defaultConnectInfo {
    ciHost      = "localhost",
    ciDatabase  = "northwind",
    ciUser      = "root",
    ciPassword  = "nataliaa"
    }
  (defs, is) <- query_ conn "SELECT * FROM orders"

  strippedVals <- map everyRow <$> (Streams.toList is)
  let o = unlines strippedVals
  putStrLn o

  where
    everyRow = intercalate "," . map (showSQLValue)
      where
        showSQLValue (MySQLInt32 i)   = "\"" ++ show i ++ "\""
        showSQLValue (MySQLText  t)   = show t
        showSQLValue (MySQLDate  d)   = "\"" ++ show d ++ "\""
        showSQLValue (MySQLDecimal d) = "\"" ++ show d ++ "\""
        showSQLValue (x)              = show x

-- lls <- Streams.toList is
-- let strippedVals = map everyRow lls

-- let lls = Streams.toList is
-- strippedVals <- map everyRow <$> lls
  