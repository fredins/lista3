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
import qualified Prelude
import Servant
import Servant.Docs (ToSample, toSamples, ToParam, toParam, ToCapture, toCapture, singleSample, DocCapture(DocCapture), DocQueryParam(DocQueryParam), ParamKind(Normal), markdown, docs)
import Data.Aeson (ToJSON)
import GHC.Generics (Generic, to, from)
import Network.Wai.Handler.Warp (run)
import Data.Foldable (find)
import Data.Maybe (fromJust, listToMaybe)
import Data.UUID.V4 (nextRandom)
import Data.UUID (nil, fromString, toASCIIBytes)
import Data.UUID.Types (UUID)
import qualified  Data.ByteString.Lazy.Char8 as LazyByteString (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Test.QuickCheck (arbitrary, Arbitrary, elements, sample, Gen) 
import Test.QuickCheck.Instances
import Agda.Utils.Maybe (caseMaybeM)
import qualified Data.ByteString as ByteString (ByteString)
import Database.PostgreSQL.Simple (query, query_, execute, execute_, connect, close, Connection, FromRow, ToRow, ConnectInfo, defaultConnectInfo, Only (Only))
import Database.PostgreSQL.Simple.ToField (ToField, toField, Action(Many, Escape))
import Database.PostgreSQL.Simple.ToRow (toRow)
import Control.Exception (bracket)
import Control.Monad (void, liftM4)
import Control.Monad.IO.Class (liftIO)
import Data.Pool (Pool, createPool, withResource)


-- DATA TYPES


newtype HelloWorld = HelloWorld String

instance ToSample HelloWorld where toSamples _ = singleSample hello

instance MimeRender PlainText HelloWorld where 
  mimeRender _ (HelloWorld x) = pack x

hello :: HelloWorld
hello = HelloWorld "hello world"

data Todo = Todo {
  id :: UUID,
  text :: String, 
  active :: Bool,
  marked :: Bool
} deriving (Eq, Show, Generic)


instance ToParam (QueryParam "active" Bool) where
  toParam _ = DocQueryParam  
    "active"  
    ["true", "false"]
    "Completion status of todos." 
    Normal

instance ToSample Todo where 
  toSamples _ = singleSample $ Todo nil "Sill" True False
 
instance ToCapture (Capture "id" UUID) where
  toCapture _ =  DocCapture 
    "id"                                
    "identification of todo"

instance ToField Todo where
  toField Todo{..} = Many [toField id, toField text, toField active, toField
    marked]

instance ToJSON Todo

instance FromRow Todo

instance ToRow Todo

instance Arbitrary Todo where
  arbitrary = liftM4 Todo arbitrary (elements ["Sill", "Dill", "Färskpotatis", 
    "Gräddfil", "Snaps"]) arbitrary arbitrary

-- SERVER

type TodoApi =  Get '[PlainText] HelloWorld
          :<|> "todos" :> QueryParam "active" Bool :> Get '[JSON] [Todo]
          :<|> "todo" :> Capture "id" UUID :> Get '[JSON] Todo

server :: Pool Connection -> Server TodoApi
server pool = return hello                 -- /
            :<|> getTodosByActivity   -- /todos?active=bool
            :<|> getTodoById          -- /todo/:id
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
      handleMaybe m = caseMaybeM m (throwError $ err404 { errBody = "Todo with ID not found." :: LazyByteString.ByteString }) return

todoAPI :: Proxy TodoApi
todoAPI = Proxy

app :: Pool Connection ->Application
app = serve todoAPI . server 

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

initConnectionPool :: ConnectInfo -> IO (Pool Connection)
initConnectionPool info = createPool (connect info) close 2 60 10

initDB :: Connection -> IO ()
initDB conn = void $ execute_ conn
  " create table if not exists todos( \
  \   id uuid,                        \
  \   text text,                      \
  \   active boolean not null,        \
  \   marked boolean not null,        \
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
selectTodoById conn id = listToMaybe <$> query conn "select * from todos where id=?" id

insertTodo :: Connection ->(UUID -> Todo) ->IO ()
insertTodo conn todo = void $ nextRandom >>= execute conn 
  "insert into todos values (?, ?, ?, ?)" . todo 

updateTodo :: Connection -> Todo ->IO ()
updateTodo conn Todo{..} = void $ execute conn 
  "update todos set id=?, text=?, active=?, marked=? where id=?" (id, text,
  active, marked, id)

deleteTables :: IO ()
deleteTables = void $ connect defaultConnectInfo >>= flip execute_"drop table todos"
