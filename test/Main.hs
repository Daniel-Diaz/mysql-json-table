
module Main (main) where

import Prelude hiding (lookup)
import Database.MySQL.Simple qualified as SQL
import Database.MySQL.JSONTable
import Data.Ratio (Ratio, (%))
import Control.Monad (unless)

main :: IO ()
main = do
  conn <- SQL.connect (SQL.defaultConnectInfo
    { SQL.connectDatabase = "json-table-test"
    , SQL.connectUser = "root"
    , SQL.connectPassword = "root"
      })
  table0 <- createTable conn True "create-delete-table"
  _ <- createTable conn False $ tableName table0
  deleteTable conn True table0
  deleteTable conn False table0
  table <- createTable conn True "int-table"
  let half :: Ratio Int
      half = 1 % 2
  x <- insert conn table half >>= lookup conn table
  unless (x == Just half) $ fail $ "Insert-Lookup test failed: got " ++ show x
