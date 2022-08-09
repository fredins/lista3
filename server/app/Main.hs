{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (id)
import qualified Prelude ()
import Servant
import Servant.Docs (ToSample, toSamples, ToParam, toParam, ToCapture, toCapture, singleSample, DocCapture(DocCapture), DocQueryParam(DocQueryParam), ParamKind(Normal), markdown, docs)
import Data.Aeson (ToJSON, toJSON, (.=), object, FromJSON, parseJSON, Value(Object, String), (.:))
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (corsRequestHeaders, simpleCorsResourcePolicy, cors, corsMethods)
import Data.Maybe (listToMaybe)
import Data.UUID.V4 (nextRandom)
import Data.UUID (nil, toASCIIBytes)
import Data.UUID.Types (UUID)
import qualified  Data.ByteString.Lazy.Char8 as LazyByteString (ByteString)
import Test.QuickCheck (arbitrary, Arbitrary, elements) 
import Test.QuickCheck.Instances ()
import Agda.Utils.Maybe (caseMaybeM)
import Database.PostgreSQL.Simple (query, query_, execute, execute_, connect, close, Connection, FromRow, ToRow, ConnectInfo, defaultConnectInfo)
import Database.PostgreSQL.Simple.ToField (ToField, toField, Action(Many, Escape))
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.ToRow (toRow)
import Control.Monad (void, liftM4, liftM3)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative (empty)
import Data.Pool (Pool, createPool, withResource)
import Data.Function.Flippers (flip4)
import Data.Text (unpack, pack)
import Distribution.Simple.Utils (lowercase)


{- TODO
 - Endpoint updateTodo if id=nil then generate new id (used for local state in frontend)
 - Endpoint newTodo should not take the active field
 - Add position field to todo, which is an integer with spaced x apart
 - Implement a hashing algorithm to redistribute positional values too close together.
 -}

-- DATA TYPES

data Todo = Todo {
  id :: UUID,
  text :: String, 
  active :: Bool,
  color :: Color
} deriving (Eq, Show, Generic)

data Color = Gray
           | Blue
           | Yellow
           | Red
           | Green 
             deriving (Eq, Generic, Show)

fromString :: String ->Color
fromString s = case lowercase s of
  "gray"   -> Gray  
  "blue"   -> Blue  
  "yellow" -> Yellow
  "red"    -> Red   
  "green"  -> Green  
  err      -> error err

newtype NewTodo = NewTodo (UUID ->Todo) deriving (Generic)

instance Arbitrary Todo where
  arbitrary = liftM4 Todo arbitrary (elements ["Sill", "Dill", "Färskpotatis", 
    "Gräddfil", "Snaps"]) arbitrary arbitrary

instance Arbitrary Color where
  arbitrary = elements [Gray, Blue, Yellow, Red, Green]

-- SERVER

type TodoApi = "todos"      :> QueryParam "active" Bool         :> Get  '[JSON]      [Todo]
          :<|> "todo"       :> Capture "id" UUID             :> Get  '[JSON]      Todo
          :<|> "newTodo"    :> ReqBody '[JSON] (UUID ->Todo) :> PostNoContent
          :<|> "updateTodo" :> ReqBody '[JSON] Todo          :> PostNoContent

instance ToJSON Todo
instance FromJSON Todo

instance ToJSON Color where 
  toJSON = String . pack . lowercase . show
  
instance FromJSON Color where
  parseJSON (String x) = return . fromString $ unpack x
  parseJSON _     = error "parseJSON Color"

instance ToParam (QueryParam "active" Bool) where
  toParam _ = DocQueryParam  
    "active"  
    ["true", "false"]
    "Activity status of the todos." 
    Normal

instance ToSample Todo where 
  toSamples _ = singleSample $ Todo nil "Sill" True Gray
 
instance ToCapture (Capture "id" UUID) where
  toCapture _ =  DocCapture 
    "id"                                
    "identification of todo"

instance ToSample (UUID ->Todo) where
  toSamples _ = singleSample $ \id ->Todo id "Sill" True Gray

instance ToJSON (UUID -> Todo) where
  toJSON newTodo = object ["text" .= text, "active" .= active, "color" .= color]
    where Todo{..} = newTodo nil

instance FromJSON (UUID -> Todo) where
  parseJSON (Object o) = liftM3 (flip4 Todo) (o .: "color") (o .: "active") (o .: "text")
  parseJSON _ = empty

server :: Pool Connection -> Server TodoApi
server pool =    getTodosByActivity   -- /todos?active=bool
            :<|> getTodoById          -- /todo/:id
            :<|> postNewTodo          -- /newTodo
            :<|> postUpdateTodo       -- /updateTodo
  where 
  toHandler = liftIO . withResource pool 

  getTodosByActivity :: Maybe Bool -> Handler [Todo]
  getTodosByActivity  = toHandler . \case 
    Just True ->selectActiveTodos
    Just False ->selectCompletedTodos
    Nothing   ->selectAllTodos

  getTodoById :: UUID -> Handler Todo
  getTodoById = handleMaybe . toHandler . flip selectTodoById
    where 
      handleMaybe :: Handler (Maybe Todo) ->Handler Todo
      handleMaybe m = caseMaybeM m (throwError $ err404 { errBody = 
       "Todo with ID not found." :: LazyByteString.ByteString }) return

  postNewTodo :: (UUID ->Todo) ->Handler NoContent
  postNewTodo newTodo =  toHandler (`insertTodo` newTodo) >> return NoContent

  postUpdateTodo :: Todo ->Handler NoContent
  postUpdateTodo todo = toHandler (`updateTodo` todo) >> return NoContent

todoAPI :: Proxy TodoApi
todoAPI = Proxy

app :: Pool Connection ->Application
app = cors' . serve todoAPI . server 
  where 
   cors' = (cors . const) $ Just simpleCorsResourcePolicy  { 
     corsRequestHeaders = ["Content-Type"], 
     corsMethods        = ["OPTIONS", "GET", "PUT", "POST"] 
   }

apiDocs :: String
apiDocs = markdown $ docs todoAPI

main :: IO ()
main = do 
  -- create pool
  pool <- initConnectionPool defaultConnectInfo 

  -- create database
  withResource pool initDB 

  -- run server
  run 8080 $ app pool

-- DATABASE

instance ToField Todo where
  toField Todo{..} = Many [toField id, toField text, toField active, toField
    color]

instance ToField Color where
  toField = toField . lowercase . show

instance FromField Color where 
  fromField f dat = fromString <$> fromField f dat

instance FromRow Todo

instance ToRow Todo

initConnectionPool :: ConnectInfo -> IO (Pool Connection)
initConnectionPool info = createPool (connect info) close 2 60 10

initDB :: Connection -> IO ()
initDB conn = void $ execute_ conn
  " create table if not exists todos( \
  \   id uuid,                        \
  \   text text,                      \
  \   active boolean not null,        \
  \   color text,                     \
  \   primary key (id)                \
  \ );"

selectAllTodos :: Connection ->IO [Todo]
selectAllTodos conn = query_ conn "select * from todos"

selectActiveTodos :: Connection -> IO [Todo]
selectActiveTodos conn = query_ conn "select * from todos where active=True"

selectCompletedTodos :: Connection -> IO [Todo]
selectCompletedTodos conn = query_ conn "select * from todos where active=False"

instance ToRow UUID where toRow uuid = [Escape $ toASCIIBytes uuid]

selectTodoById :: Connection ->UUID ->IO (Maybe Todo)
selectTodoById conn id = listToMaybe <$> query conn 
  "select * from todos where id=?" id

insertTodo :: Connection ->(UUID -> Todo) ->IO ()
insertTodo conn todo = void $ nextRandom >>= execute conn 
  "insert into todos values (?, ?, ?, ?)" . todo  

updateTodo :: Connection -> Todo ->IO ()
updateTodo conn Todo{..} = void $ execute conn 
  "update todos set id=?, text=?, active=?, color=? where id=?" (id, text,
  active, color, id)

deleteTables :: IO () 
deleteTables = void $ connect defaultConnectInfo >>= flip execute_"drop table todos"
