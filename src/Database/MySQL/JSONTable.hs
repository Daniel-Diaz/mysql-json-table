----------------------------------------------------------------------------------------------------
module Database.MySQL.JSONTable
  ( ID
  , JSONTable (..)
  , createTable
  , deleteTable
    ) where

import Data.Word
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Database.MySQL.Simple qualified as SQL

-- | Row identifier used for table lookups.
--   The type parameter indicates the type of data
--   stored in the table.
newtype ID a = ID { fromID :: Word64 }

-- | A MySQL table with two columns:
--
-- +----------------------------+-----------+
-- |             id             |    data   |
-- +============================+===========+
-- | Row identifier (type 'ID') | JSON data |
-- +----------------------------+-----------+
--
-- The type parameter indicates the type of data
--  stored in the table.
data JSONTable a = JSONTable
  { -- | Table name.
    tableName :: Text
    }

tableSpecs :: String
tableSpecs = concat
  [ "("
  , "id BIGINT UNSIGNED NOT NULL PRIMARY KEY AUTO_INCREMENT"
  , ", "
  , "data JSON"
  , ")"
    ]

-- | Create a new JSON table in a MySQL database.
createTable
  :: SQL.Connection -- ^ MySQL database connection.
  -> Bool -- ^ Fail if table already exists.
  -> Text -- ^ Table name.
  -> IO (JSONTable a)
createTable conn failIfExists name = do
  let ifNotExists = if failIfExists then " " else " IF NOT EXISTS "
      query = "CREATE TABLE" ++ ifNotExists ++ "`" ++ Text.unpack name ++ "` " ++ tableSpecs
  _ <- SQL.execute conn (fromString query) ()
  pure $ JSONTable
    { tableName = name
      }

-- | Delete a JSON table from a MySQL database.
deleteTable
  :: SQL.Connection -- ^ MySQL database connection.
  -> Bool -- ^ Fail if table doesn't exist.
  -> JSONTable a 
  -> IO ()
deleteTable conn failIfNotExist table = do
  let ifExists = if failIfNotExist then " " else " IF EXISTS "
      query = "DROP TABLE" ++ ifExists ++ "`" ++ Text.unpack (tableName table) ++ "`"
  _ <- SQL.execute conn (fromString query) ()
  pure ()
