
module Main (main) where

import Database.MySQL.Simple qualified as SQL
import Database.MySQL.JSONTable

main :: IO ()
main = do
  conn <- SQL.connect (SQL.defaultConnectInfo
    { SQL.connectDatabase = "json-table-test"
      })
  table <- createTable conn True "int-table"
  _ <- createTable conn False $ tableName table
  pure ()
