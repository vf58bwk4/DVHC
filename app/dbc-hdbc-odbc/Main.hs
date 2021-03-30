module Main where

import Database.HDBC
import Database.HDBC.Types
import Database.HDBC.ODBC
import Data.List
import Data.List.Split

main :: IO () 
main = do
  conn <- connectODBC "DSN=MySQL: northwind"
  rows <- quickQuery conn "SELECT * FROM categories" []
  -- print vals
  let strippedVals = map convertRow rows
  print strippedVals
  disconnect conn
  -- print "Done."
  where  
    convertRow = intercalate "," . map (showSQLValue)
      where
        showSQLValue (SqlInt32 i)       = show i
        showSQLValue (SqlByteString bs) = show bs
        showSQLValue (SqlLocalDate ld)  = show ld