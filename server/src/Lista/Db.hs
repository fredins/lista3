{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Lista.Db (module Lista.Db) where

import           Data.Aeson                           (FromJSON, ToJSON,
                                                       Value (Object, String),
                                                       object, parseJSON,
                                                       toJSON, (.:), (.=))
import           Data.Pool                            (Pool, createPool)
import qualified Data.Text                            as ST
import           Data.UUID                            (nil, toASCIIBytes)
import           Data.UUID.Types                      (UUID)
import           Data.UUID.V4                         (nextRandom)
import           Database.PostgreSQL.Simple           (ConnectInfo, Connection,
                                                       FromRow, ToRow, close,
                                                       connect,
                                                       defaultConnectInfo,
                                                       execute, execute_, query,
                                                       query_)
import           Database.PostgreSQL.Simple.FromField (FromField, fromField)
import           Database.PostgreSQL.Simple.SqlQQ     (sql)
import           Database.PostgreSQL.Simple.ToField   (Action (Escape, Many),
                                                       ToField, toField)
import           Database.PostgreSQL.Simple.ToRow     (toRow)
import           Relude                               hiding (id)
import           Servant.Docs                         (ToSample, singleSample,
                                                       toSamples)
import           Test.QuickCheck                      (Arbitrary, arbitrary,
                                                       elements)
import           Test.QuickCheck.Instances            ()



initConnectionPool :: ConnectInfo -> IO (Pool Connection)
initConnectionPool info = createPool (connect info) close 2 60 10

initDB :: Connection -> IO ()
initDB conn = void $ execute_ conn
  [sql|
    create table if not exists users(
      id text,
      name text,
      email text,
      primary key (id)
    );

    create table if not exists sessions(
      sessionKey text,
      userId text,
      primary key (sessionKey),
      foreign key (userId) references users(id)
    );

    create table if not exists lists(
      id uuid,
      name text,
      primary key (id)
    );

    create table if not exists listAccess(
      listId uuid,
      userId text,
      primary key (listId, userId),
      foreign key (userId) references users(id),
      foreign key (listId) references lists(id)
    );

    create table if not exists todos(
      id uuid,
      text text,
      active boolean not null,
      color text,
      listId uuid,
      primary key (id),
      foreign key (listId) references lists(id)
    );
  |]


initMockData :: IO ()
initMockData = void $ connect defaultConnectInfo >>= \conn -> do
  todoId <- nextRandom
  listId <- nextRandom
  let
    userId = "user-id"
    user = User {id=userId, name="martin", email="martin@fredin.org"}
    list = List {id=listId, name="martins list"}
    listAccess = ListAccess {listId=listId, userId=userId}
    todo = Todo {id=todoId, text="Sill", active=True, color=Blue, listId=listId}

  insertUser conn user
  _ <- insertList' conn list
  insertListAccess conn listAccess
  insertTodo' conn todo


deleteTables :: IO ()
deleteTables = void $ connect defaultConnectInfo >>= flip execute_
  "drop table if exists todos, lists, users, listAccess, sessions"


---------------------------------------------
-- Session
---------------------------------------------

data Session = Session
  { sessionKey :: Text
  , userId     :: Text
  } deriving (Show, Generic)

instance FromRow Session

instance ToRow Session

selectSession :: Connection -> Text -> IO (Maybe Session)
selectSession conn = fmap listToMaybe . query conn
  "select * from sessions where sessionKey=?"

insertSession :: Connection -> Session -> IO ()
insertSession conn = void . execute conn "insert into sessions values (?, ?)"


---------------------------------------------
-- User
---------------------------------------------

data User = User
  { id    :: Text -- sub
  , name  :: Text
  , email :: Text
  } deriving (Show, Generic)

instance FromJSON User

instance ToJSON User

instance FromRow User

instance ToRow User

instance ToSample User where
  toSamples _ =  singleSample $ User "" "name" "name@domain.org"


insertUser :: Connection -> User -> IO ()
insertUser conn = void . execute conn "insert into users values (?, ?, ?)"

selectUserById :: Connection -> Text -> IO (Maybe User)
selectUserById conn id = listToMaybe <$> query conn
  "select * from users where id=?" id



---------------------------------------------
-- ListAccess
---------------------------------------------

data ListAccess = ListAccess
  { listId :: UUID
  , userId :: Text
  } deriving (Show, Generic)

instance FromJSON ListAccess

instance ToJSON ListAccess

instance FromRow ListAccess

instance ToRow ListAccess

insertListAccess :: Connection -> ListAccess -> IO ()
insertListAccess conn = void . execute conn "insert into listAccess values (?, ?)"

selectListAccess :: Connection -> Text -> IO [ListAccess]
selectListAccess conn = query conn "select * from listAccess where userId=?"

---------------------------------------------
-- List
---------------------------------------------

data List = List
  { id   :: UUID
  , name :: Text
  } deriving (Show, Generic)

instance ToJSON List

instance FromJSON List

instance FromRow List

instance ToRow List

instance ToSample List where
  toSamples _ = singleSample $ List nil "min lista"

selectAllLists :: Connection -> Text -> IO [List]
selectAllLists conn userId = do
  (xs :: [JoinListListAccess]) <- query_ conn
    [sql|
      select listId, userId, name from lists
      join listAccess on lists.id=listAccess.listId
    |]

  let listId' = listId :: JoinListListAccess -> UUID
  concat <$> mapM (selectListById' conn  . listId') xs

selectListById' :: Connection -> UUID -> IO [List]
selectListById' conn = query conn "select * from lists where id=?"

selectListById :: Connection -> UUID -> IO (Maybe List)
selectListById conn listId = listToMaybe <$> selectListById' conn listId

insertList :: Connection -> Text -> Text -> IO ()
insertList conn userId name = insertList' conn . (`List` name) =<< nextRandom

insertList' :: Connection -> List -> IO ()
insertList' conn = void . execute conn "insert into lists values (?, ?)"

---------------------------------------------
-- Todo
---------------------------------------------

data Todo = Todo
  { id     :: UUID
  , text   :: Text
  , active :: Bool
  , color  :: Color
  , listId :: UUID
  } deriving (Eq, Show, Generic)


instance FromJSON Todo

instance FromJSON (UUID -> Todo) where
  parseJSON (Object o) = do
    text   <- o .: "text"
    active <- o .: "active"
    color  <- o .: "color"
    listId <- o .: "listId"
    return (\id -> Todo id text active color listId)
  parseJSON _ = empty

instance ToJSON Todo

instance ToJSON (UUID -> Todo) where
  toJSON newTodo = object ["text" .= text, "active" .= active, "color" .= color]
    where Todo{..} = newTodo nil

instance ToField Todo where
  toField Todo{..} = Many [toField id, toField text, toField active, toField color]

instance FromRow Todo

instance ToRow Todo

instance ToSample Todo where
  toSamples _ = singleSample $ Todo nil "Sill" True Gray nil

instance ToSample (UUID -> Todo) where
  toSamples _ = singleSample $ \id -> Todo id "Sill" True Gray nil

instance Arbitrary Todo where
  arbitrary = do
    text <-elements ["Sill", "Dill", "Färskpotatis", "Gräddfil", "Snaps"]
    active <-arbitrary
    color <-arbitrary
    return (Todo nil text active color nil)


selectAllTodos :: Connection -> UUID -> IO [Todo]
selectAllTodos conn = query conn "select * from todos where listId=?"

selectActiveTodos :: Connection -> UUID -> IO [Todo]
selectActiveTodos conn = query conn "select * from todos where active=True and listId=?"

selectCompletedTodos :: Connection -> UUID -> IO [Todo]
selectCompletedTodos conn = query conn "select * from todos where active=False and listId=?"

selectTodoById :: Connection -> UUID -> IO (Maybe  Todo)
selectTodoById conn id = listToMaybe <$> query conn "select * from todos where id=?" id

insertTodo :: Connection -> (UUID -> Todo) -> IO ()
insertTodo conn mkTodo = nextRandom >>= insertTodo' conn . mkTodo

insertTodo' :: Connection -> Todo -> IO ()
insertTodo' conn = void . execute conn "insert into todos values (?, ?, ?, ?, ?)"

updateTodo :: Connection -> Todo -> IO ()
updateTodo conn Todo{..} = void $ execute conn
  "update todos set id=?, text=?, active=?, color=? where id=?" (id, text, active, color,
  id)

---------------------------------------------
-- Color
---------------------------------------------

data Color = Gray
           | Blue
           | Yellow
           | Red
           | Green
             deriving (Eq, Generic, Show)

instance ToJSON Color where
  toJSON = String . ST.toLower . show

instance FromJSON Color where
  parseJSON (String x) = return $ Lista.Db.fromString x
  parseJSON _          = error "parseJSON Color"

instance ToField Color where
  toField = (toField :: Text -> Action). show

instance FromField Color where
  fromField f dat = Lista.Db.fromString <$> fromField f dat

instance Arbitrary Color where
  arbitrary = elements [Gray, Blue, Yellow, Red, Green]

fromString :: Text -> Color
fromString x = case ST.toLower x of
  "gray"   -> Gray
  "blue"   -> Blue
  "yellow" -> Yellow
  "red"    -> Red
  "green"  -> Green
  err      -> error err



---------------------------------------------
-- Other
---------------------------------------------

instance ToRow Text where
 toRow a = [Escape $ encodeUtf8 a]

instance ToRow UUID where toRow uuid = [Escape $ toASCIIBytes uuid]

data JoinListListAccess = JoinListListAccess
  { listId :: UUID
  , userId :: Text
  , name   :: Text
  } deriving (Show, Generic)

instance FromRow JoinListListAccess
