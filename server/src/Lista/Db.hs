{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Lista.Db (module Lista.Db) where

import           Data.Aeson                           (FromJSON, ToJSON,
                                                       Value (Object, String),
                                                       object, parseJSON,
                                                       toJSON, (.:), (.=))
import           Data.Foldable.Extra                  (notNull)
import           Data.Maybe                           (fromJust)
import           Data.Pool                            (Pool, createPool)
import qualified Data.Text                            as ST
import           Data.UUID                            (nil, toASCIIBytes)
import           Data.UUID.Types                      (UUID)
import           Data.UUID.V4                         (nextRandom)
import           Database.PostgreSQL.Simple           (ConnectInfo, Connection,
                                                       FromRow, ToRow, close,
                                                       connect,
                                                       defaultConnectInfo,
                                                       execute, executeMany,
                                                       execute_, query, query_)
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

    create table if not exists invitations(
      id uuid,
      listId uuid,
      invitedId text,
      owner text,
      listName text,
      primary key (id),
      foreign key (listId) references lists(id),
      foreign key (invitedId) references users(id)
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
  "drop table if exists todos, lists, users, listAccess, sessions, invitations"


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

selectUserByEmail :: Connection -> Text -> IO [User]
selectUserByEmail conn = query conn "select * from users where email=?"

data UserDetails = UserDetails
  { name  :: Text
  , email :: Text
  } deriving (Show, Generic)

instance ToJSON UserDetails

instance FromJSON UserDetails

instance ToRow UserDetails

instance FromRow UserDetails

selectUserDetailsById :: Connection -> Text -> IO (Maybe UserDetails)
selectUserDetailsById conn id = listToMaybe <$> query conn
  "select name, email from users where id=?" id

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

checkListAccess :: Connection -> Text -> UUID -> IO Bool
checkListAccess conn userId listId = notNull <$> (query conn
  "select * from listAccess where userId=? and listId=?"
  (userId, listId) :: IO [ListAccess])

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


data JoinListListAccess = JoinListListAccess
  { listId :: UUID
  , userId :: Text
  , name   :: Text
  } deriving (Show, Generic)

instance FromRow JoinListListAccess

selectAllLists :: Connection -> Text -> IO [List]
selectAllLists conn userId = do
  (xs :: [JoinListListAccess]) <- query conn
    [sql|
      select listId, userId, name from lists
      join listAccess on lists.id=listAccess.listId
      where userId=?
    |] userId

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

deleteList :: Connection -> UUID -> IO ()
deleteList conn listId = void $ execute conn
  [sql|
    delete from listAccess where listId=?;
    delete from todos where listId=?;
    delete from lists where id=?;
    delete from invitations where listId=?
  |] (listId, listId, listId, listId)

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
instance ToJSON Todo
instance ToField Todo where
  toField Todo{..} = Many [toField id, toField text, toField active, toField color, toField listId]
instance FromRow Todo
instance ToRow Todo
instance Arbitrary Todo where
  arbitrary = do
    text <-elements ["Sill", "Dill", "Färskpotatis", "Gräddfil", "Snaps"]
    active <-arbitrary
    color <-arbitrary
    return (Todo nil text active color nil)

data NewTodo = NewTodo
  { text   :: Text
  , active :: Bool
  , color  :: Color
  , listId :: UUID
  } deriving (Show, Generic)

instance FromJSON NewTodo
instance ToJSON NewTodo

instance ToSample Todo where
  toSamples _ = singleSample $ Todo nil "Sill" True Gray nil

selectAllTodos :: Connection -> UUID -> IO [Todo]
selectAllTodos conn = query conn "select * from todos where listId=?"

selectActiveTodos :: Connection -> UUID -> IO [Todo]
selectActiveTodos conn = query conn "select * from todos where active=True and listId=?"

selectCompletedTodos :: Connection -> UUID -> IO [Todo]
selectCompletedTodos conn = query conn "select * from todos where active=False and listId=?"

selectTodoById :: Connection -> UUID -> IO (Maybe  Todo)
selectTodoById conn id = listToMaybe <$> query conn "select * from todos where id=?" id

insertTodo :: Connection -> NewTodo -> IO ()
insertTodo conn NewTodo{..} = nextRandom >>= insertTodo' conn .
  (\id -> Todo {id=id, text=text, active=active, color=color, listId=listId})

insertTodo' :: Connection -> Todo -> IO ()
insertTodo' conn = void . execute conn "insert into todos values (?, ?, ?, ?, ?)"

updateTodo :: Connection -> Todo -> IO ()
updateTodo conn Todo{..} = void $ execute conn
  "update todos set id=?, text=?, active=?, color=? where id=?" (id, text, active, color,
  id)

deleteTodo :: Connection -> UUID -> IO ()
deleteTodo conn = void . execute conn "delete from todos where id=?"

---------------------------------------------
-- Invitation
---------------------------------------------

data Invitation = Invitation
  { id        :: UUID
  , listId    :: UUID
  , invitedId :: Text
  , owner     :: Text
  , listName  :: Text
  } deriving (Show, Generic)

instance FromJSON Invitation

instance ToJSON Invitation

instance FromRow Invitation

instance ToRow Invitation

data InvitationDetails = InvitationDetails
  { id       :: UUID
  , listId   :: UUID
  , owner    :: Text
  , listName :: Text
  } deriving (Show, Generic)

instance FromJSON InvitationDetails

instance ToJSON InvitationDetails

instance FromRow InvitationDetails

instance ToRow InvitationDetails

insertInvitation :: Connection -> Text -> UUID -> Text -> IO ()
insertInvitation conn ownerId listId email = do
  User { name=owner } <- fromJust <$> selectUserById conn ownerId
  List { id=listId, name=listName } <- fromJust <$> selectListById conn listId

  let
    -- Check if user doesn't already have an invitation or access to the list
    checkUser User{id} = do
      inv <- selectInvitationByInvitedId conn listId id
      newMember <- not <$> checkListAccess conn id listId
      pure (isNothing inv && newMember)

    mkInvitation User{ id=invitedId } = do
      invitationId <- nextRandom
      pure Invitation
        { id        = invitationId
        , listId    = listId
        , invitedId = invitedId
        , owner     = owner
        , listName  = listName
        }

  us <- filterM checkUser =<< selectUserByEmail conn email
  is <- mapM mkInvitation us
  void $ executeMany conn "insert into invitations values (?, ?, ?, ?, ?)" is

selectInvitationDetails :: Connection -> Text -> IO [InvitationDetails]
selectInvitationDetails conn = query conn
  "select id, listId, owner, listName from invitations where invitedId=?"

selectInvitationByInvitedId :: Connection -> UUID -> Text -> IO (Maybe Invitation)
selectInvitationByInvitedId conn listId invitedId = listToMaybe <$> query conn
  "select * from invitations where listId=? and invitedId=?" (listId, invitedId)

selectInvitation :: Connection -> UUID -> Text -> IO [Invitation]
selectInvitation conn listId email = query conn
  "select * from invitations where listId=? and email=?" (listId, email)

selectInvitationDetailsById :: Connection -> UUID -> IO (Maybe InvitationDetails)
selectInvitationDetailsById conn invitationId = listToMaybe <$> query conn
  "select id, listId, owner, listName from invitations where id=?" invitationId

selectInvitationById :: Connection -> UUID -> IO (Maybe Invitation)
selectInvitationById conn invitationId = listToMaybe <$> query conn
  "select * from invitations where id=?" invitationId

deleteInvitation :: Connection -> UUID -> IO ()
deleteInvitation conn invitationId = void $ execute conn
    "delete from invitations where id=?" invitationId


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

