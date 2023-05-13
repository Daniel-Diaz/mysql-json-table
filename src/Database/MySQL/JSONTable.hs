----------------------------------------------------------------------------------------------------
module Database.MySQL.JSONTable
  ( -- * Types
    Id
  , JSONTable (..)
    -- * Table operations
  , createTable
  , deleteTable
    -- * Row operations
  , insert
  , lookup
  , adjust
  , delete
    ) where

import Prelude hiding (lookup)
import Data.Word
import Data.String (fromString)
import Data.Maybe (listToMaybe)
import Data.Typeable (Typeable)
import Control.Monad (forM_)
import Data.ByteString qualified as ByteString
import Database.MySQL.Simple qualified as SQL
import Database.MySQL.Base.Types qualified as SQLTypes
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as JSON

-- | Row identifier used for table lookups.
--   The type parameter indicates the type of data
--   stored in the table.
newtype Id a = Id { fromId :: Word64 } deriving (Eq, Ord, Show)

instance SQL.ToField (Id a) where
  toField = fromString . show . fromId

instance SQL.Param (Id a)

-- | A MySQL table with two columns:
--
-- +----------------------------+-----------+
-- |             id             |    data   |
-- +============================+===========+
-- | Row identifier (type 'Id') | JSON data |
-- +----------------------------+-----------+
--
-- The type parameter indicates the type of data
--  stored in the table.
data JSONTable a = JSONTable
  { -- | Table name.
    tableName :: String
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
  -> String -- ^ Table name.
  -> IO (JSONTable a)
createTable conn failIfExists name = do
  let ifNotExists = if failIfExists then " " else " IF NOT EXISTS "
      query = "CREATE TABLE" ++ ifNotExists ++ "`" ++ name ++ "` " ++ tableSpecs
  _ <- SQL.execute conn (fromString query) ()
  pure $ JSONTable
    { tableName = name
      }

-- | Delete a JSON table from a MySQL database, together with all of its content.
deleteTable
  :: SQL.Connection -- ^ MySQL database connection.
  -> Bool -- ^ Fail if table doesn't exist.
  -> JSONTable a 
  -> IO ()
deleteTable conn failIfNotExist table = do
  let ifExists = if failIfNotExist then " " else " IF EXISTS "
      query = "DROP TABLE" ++ ifExists ++ "`" ++ tableName table ++ "`"
  _ <- SQL.execute conn (fromString query) ()
  pure ()

-- | JSON serialization helper.
newtype AsJSON a = AsJSON { asJSON :: a }

instance FromJSON a => SQL.FromField (AsJSON a) where
  fromField = ([SQLTypes.Json], fmap AsJSON . JSON.eitherDecodeStrict)

instance ToJSON a => SQL.ToField (AsJSON a) where
  toField = ByteString.toStrict . JSON.encode . asJSON

instance (Typeable a, FromJSON a) => SQL.Result (AsJSON a)
instance ToJSON a => SQL.Param (AsJSON a)

-- | Insert a new row into a table.
insert
  :: ToJSON a
  => SQL.Connection -- ^ MySQL database connection.
  -> JSONTable a -- ^ Table to insert the new row.
  -> a -- ^ Data for the new row.
  -> IO (Id a) -- ^ Identifier of the new row.
insert conn table x = do
  let query = "INSERT INTO `" ++ tableName table ++ "` (data) VALUES (?)"
  _ <- SQL.execute conn (fromString query) $ SQL.Only $ AsJSON x
  Id <$> SQL.insertID conn

-- | Lookup a row in a table.
lookup
  :: (Typeable a, FromJSON a)
  => SQL.Connection -- ^ MySQL database connection.
  -> JSONTable a -- ^ Table for lookup.
  -> Id a -- ^ Identifier to use for the table lookup.
  -> IO (Maybe a)
lookup conn table i = do
  let query = "SELECT data FROM `" ++ tableName table ++ "` WHERE id=?"
  fmap (asJSON . SQL.fromOnly) . listToMaybe <$> SQL.query conn (fromString query) (SQL.Only i)

-- | Update a row by applying the supplied function. If the row doesn't exist,
--   it does nothing.
adjust
  :: (Typeable a, FromJSON a, ToJSON a)
  => SQL.Connection -- ^ MySQL database connection.
  -> JSONTable a
  -> (a -> IO a) -- ^ Update function.
  -> Id a
  -> IO ()
adjust conn table f i = SQL.withTransaction conn $ do
  let query1 = "SELECT data FROM `" ++ tableName table ++ "` WHERE id=? FOR SHARE"
  mr <- listToMaybe <$> SQL.query conn (fromString query1) (SQL.Only i)
  forM_ mr $ \(SQL.Only (AsJSON x)) -> do
    y <- f x
    let query2 = "UPDATE `" ++ tableName table ++ "` SET data=? WHERE id=?"
    _ <- SQL.execute conn (fromString query2) (AsJSON y,i)
    pure ()

-- | Delete a row from a table. It does nothing if the row doesn't exist.
delete
  :: SQL.Connection -- ^ MySQL database connection.
  -> JSONTable a -- ^ Table to delete the row from.
  -> Id a -- ^ Identifier of the row to delete.
  -> IO ()
delete conn table i = do
  let query = "DELETE FROM `" ++ tableName table ++ "` WHERE id=?"
  _ <- SQL.execute conn (fromString query) $ SQL.Only i
  pure ()
