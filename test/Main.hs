
module Main (main) where

import Database.MySQL.Simple qualified as SQL
import Database.MySQL.JSONTable

main :: IO ()
main = do
  conn <- SQL.connect (SQL.defaultConnectInfo
    { SQL.connectDatabase = "json-table-test"
    , SQL.connectUser = "root"
    , SQL.connectPassword = "root"
      })
  table <- createTable conn True "create-delete-table"
  _ <- createTable conn False $ tableName table
  deleteTable conn True table
  deleteTable conn False table
