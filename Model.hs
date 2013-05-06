module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Database.Persist.Store (PersistValue, SqlType)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
  
newtype DoM = DoM { getDay :: Int } deriving (Show, Eq, Read)
newtype MoY = MoY { getMth :: Int } deriving (Show, Eq, Read)

-- store Day values in the database as an Int
instance PersistField DoM where
  toPersistValue    = (toPersistValue :: Int -> PersistValue) . getDay 
  fromPersistValue val = 
    case (fromPersistValue :: PersistValue -> Either Text Int) val of
      (Left text) -> Left text
      (Right i)   -> Right (DoM i)
      
  sqlType day = (sqlType :: Int -> SqlType) (getDay day)
  isNullable _ = False
                 
-- store Mth values in the database as an Int
instance PersistField MoY where
  toPersistValue   = (toPersistValue :: Int -> PersistValue) . getMth
  fromPersistValue val = 
    case (fromPersistValue :: PersistValue -> Either Text Int) val of
      (Left text) -> Left text
      (Right i)   -> Right (MoY i)
      
  sqlType = (sqlType :: Int -> SqlType) . getMth
  isNullable _ = False
  
-- route paths same as an Int
instance PathPiece DoM where
  fromPathPiece t = (fromPathPiece :: Text -> Maybe Int) t >>= Just . DoM
  toPathPiece     = (toPathPiece :: Int -> Text) . getDay 
  
-- route paths same as an Int
instance PathPiece MoY where
  fromPathPiece t = (fromPathPiece :: Text -> Maybe Int) t >>= Just . MoY
  toPathPiece     = (toPathPiece :: Int -> Text) . getMth