{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main (module Main) where

import           Data.Aeson                       (ToJSON)
import           Data.List                        (lookup)
import           Data.Pool                        (Pool, withResource)
import           Data.UUID.Types                  (UUID)
import           Database.PostgreSQL.Simple       (Connection,
                                                   defaultConnectInfo)
import           Lista.Auxiliary
import           Lista.Db                         as D
import           Lista.Oidc                       as O
import           Lista.ServerError
import qualified Prelude                          as P

import qualified Data.ByteString.Char8            as BC
import           Data.UUID.V4                     (nextRandom)
import           Network.HTTP.Client              (Manager, newManager)
import           Network.HTTP.Client.TLS          (tlsManagerSettings)
import           Network.HTTP.Types.URI           (queryToQueryText)
import           Network.Wai                      (Request, queryString,
                                                   requestHeaders)
import           Network.Wai.Handler.Warp         (defaultSettings, run,
                                                   setPort)
import           Network.Wai.Handler.WarpTLS      (runTLS, tlsSettings)
import           Network.Wai.Middleware.Cors      (cors, corsMethods,
                                                   corsOrigins,
                                                   corsRequestHeaders,
                                                   simpleCorsResourcePolicy)
import           Paths_lista                      (getDataFileName)
import           Relude
import           Servant
import           Servant.HTML.Blaze               (HTML)
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)
import           Web.Cookie                       (SetCookie, def, parseCookies,
                                                   sameSiteLax, sameSiteNone,
                                                   setCookieDomain,
                                                   setCookieHttpOnly,
                                                   setCookieName, setCookiePath,
                                                   setCookieSameSite,
                                                   setCookieSecure,
                                                   setCookieValue)

oidcConf :: ByteString -> OidcConf
oidcConf password = OidcConf
  { redirectUri = "https://lista.fredin.org/server/login/cb"
  , clientId = "223213082722-843t5m7qmtevvlor7u3vmn3gophp22eq.apps.googleusercontent.com"
  , clientPassword = password
  }

main :: IO ()
main = do
  pool     <- initConnectionPool defaultConnectInfo
  mgr      <- newManager tlsManagerSettings
  password <- P.head . BC.lines <$> (readFileBS =<< getDataFileName "secrets")
  oidcEnv  <- initOidc $ oidcConf password
  withResource pool initDB
  let context' = context pool mgr
      server'  = server  pool mgr oidcEnv

  runApplication $ serveWithContext api context' server'

runApplication :: Application -> IO ()
runApplication app = do
  let settings = setPort 4000 defaultSettings
  tls <- liftA2 tlsSettings (getDataFileName "./ssl/cert.pem") $ getDataFileName "./ssl/key.pem"
  runTLS tls settings app

-- TODO
instance HasContextEntry '[] (AuthHandler Request Session) where
  getContextEntry _ = error "error: getContextEntry"

type instance AuthServerData (AuthProtect "cookie-auth") = Session

context :: Pool Connection -> Manager -> Context '[AuthHandler Request Session]
context pool mgr = authHandler pool mgr :. EmptyContext

authHandler :: Pool Connection -> Manager -> AuthHandler Request Session
authHandler pool mgr = mkAuthHandler checkCookie
  where
  checkCookie :: Request -> Handler Session
  checkCookie req = do
    sessionKey <- maybeToRightM (unauthorizedErr "No sessionKey in cookies.") $
                  lookupCookie req "sessionKey"
    session sessionKey

  checkQueryParam :: Request -> Handler Session
  checkQueryParam req = do
    sessionKey <- maybeToRightM (unauthorizedErr "No sessionKey in query paramas")
                  . join . lookup "sessionKey" . queryToQueryText $ queryString req
    session sessionKey

  session :: Text -> Handler Session
  session sessionKey = do
    msession <- liftIO $ withResource pool (`selectSession` sessionKey)
    maybeToRightM (unauthorizedErr "No valid Session for sessionKey.") msession

lookupCookie :: Request -> ByteString -> Maybe Text
lookupCookie req name = do
  cookies <- lookup "cookie" $ requestHeaders req
  cookie <- lookup name $ parseCookies cookies
  pure (decodeUtf8 cookie)

api :: Proxy Api
api = Proxy

type Api = "server" :> (
           Get '[HTML] Homepage
      :<|> OidcApi SuccessPage
      :<|> "private"      :> AuthProtect "cookie-auth" :> PrivateApi
      :<|> "ping"         :> Get '[PlainText] Text
      :<|> "authenticate" :> QueryParam "sessionKey" Text :> Get '[JSON] (CookieHeader UserDetails))


type CookieHeader = Headers '[Header "Set-Cookie" SetCookie]

server :: Pool Connection -> Manager -> OidcEnv -> Server Api
server pool mgr oidcEnv = pure Homepage
                     :<|> oidcServer oidcEnv (handleSuccessfulLoggedIn pool)
                     :<|> privateServer pool
                     :<|> pure "Pong.\n"
                     :<|> handleAuthenticate pool

handleSuccessfulLoggedIn :: Pool Connection -> AuthInfo -> Handler SuccessPage
handleSuccessfulLoggedIn pool AuthInfo{..} = do
  sessionKey <- decodeUtf8 <$> liftIO genRandomBS
  -- Check if new user and act accordingly
  liftIO . withResource pool $ \conn -> do
    -- Get/create user
    user@D.User {id=userId} <- whenNothingM (selectUserById conn sub) $ do
      let user = D.User {id=sub, name=name, email=email}
      insertUser conn user
      pure user

    -- Create session
    whenNothingM_ (selectSession conn sessionKey) .
      D.insertSession conn $
      D.Session {sessionKey=sessionKey, userId=userId}

  pure $ SuccessPage sessionKey

handleAuthenticate :: Pool Connection -> Maybe Text -> Handler (CookieHeader UserDetails)
handleAuthenticate _    Nothing           =  preconditionFailed "Missing query param sessionKey."
handleAuthenticate pool (Just sessionKey) = do
  msession <- liftIO . withResource pool $ (`selectSession` sessionKey)
  Session{sessionKey, userId} <- maybeToRightM (unauthorizedErr "No valid Session for sessionKey.") msession
  muser <- liftIO . withResource pool $ (`selectUserById` userId)
  userDetails <- maybeToRightM (serverErrorErr "No User!") (userToUserDetails <$> muser)

  let cookie = def
       { setCookieName = "sessionKey"
       , setCookieValue = encodeUtf8 sessionKey
       -- , setCookieHttpOnly = True
       , setCookieSameSite = Just sameSiteLax
       , setCookiePath = Just "/"
       -- , setCookieSecure = False
       -- , setCookieDomain = Just "https://lista.fredin.org"
       }
  pure $ addHeader cookie userDetails

type PrivateApi = "userDetails" :> Get '[JSON] UserDetails

             :<|> "lists"       :> Get '[JSON] [List]

             :<|> "newList"     :> QueryParam "name" Text
                                :> Get '[JSON] List

             :<|> "todos"       :> QueryParam "listId" UUID
                                :> QueryParam "active" Bool
                                :> Get '[JSON] [Todo]

             :<|> "newTodo"     :> ReqBody '[JSON] NewTodo
                                :> Post '[JSON] NoContent

             :<|> "updateTodo"  :> ReqBody '[JSON] Todo
                                :> Post '[JSON] NoContent

             :<|> "deleteList"  :> QueryParam' '[Required] "listId" UUID
                                :> Get '[JSON] NoContent


privateServer :: Pool Connection -> Session -> Server PrivateApi
privateServer pool Session{..} = userDetails
                            :<|> lists
                            :<|> newList
                            :<|> todos
                            :<|> newTodo
                            :<|> updateTodo
                            :<|> deleteList
  where
  userDetails :: Handler UserDetails
  userDetails = do
    muser <- liftIO $ withResource pool (`selectUserById` userId)
    maybeToRightM (serverErrorErr "No User!") (userToUserDetails <$> muser)

  lists :: Handler [List]
  lists = liftIO $ withResource pool (`selectAllLists` userId)

  newList :: Maybe Text -> Handler List
  newList mname = do
    n <- maybeToRightM (preconditionFailedErr "Missing query param name.") mname

    -- Check if list already exists
    lists <- liftIO $ withResource pool (`selectAllLists` userId)
    let name' = name :: List -> Text
    when (any ((==n) . name') lists) $
      preconditionFailed "List already exists!"

    -- Create list and listAccess
    liftIO . withResource pool $ \conn -> do
      listId <- nextRandom
      let newList = List {id=listId, name=n}
      insertList' conn newList
      insertListAccess conn ListAccess {listId=listId, userId=userId}
      pure newList

  todos :: Maybe UUID -> Maybe Bool -> Handler [Todo]
  todos mlistId mactive = do
    listId <- maybeToRightM (preconditionFailedErr "Missing query param listId.") mlistId
    liftIO . withResource pool $ \conn ->
      case mactive of
        Nothing    -> selectAllTodos conn listId
        Just True  -> selectActiveTodos conn listId
        Just False -> selectCompletedTodos conn listId

  newTodo :: NewTodo -> Handler NoContent
  newTodo mkTodo = do
    liftIO $ withResource pool (`insertTodo` mkTodo)
    pure NoContent

  updateTodo :: Todo -> Handler NoContent
  updateTodo todo = do
      liftIO $ withResource pool (`D.updateTodo` todo)
      pure NoContent

  deleteList :: UUID -> Handler NoContent
  deleteList listId = do
    liftIO $ withResource pool (`D.deleteList` listId)
    pure NoContent

userToUserDetails :: User -> UserDetails
userToUserDetails User{name, email} = UserDetails {name=name, email=email}

data UserDetails = UserDetails
  { name  :: Text
  , email :: Text
  } deriving (Show, Generic)

instance ToJSON UserDetails

newtype UUID' = UUID'
  { id :: UUID
  } deriving (Show, Generic)

corsMiddleware = (cors . const) $ Just simpleCorsResourcePolicy
  { corsOrigins        = Just (["http://localhost:3000"], True)
  , corsRequestHeaders = ["Content-Type"]
  , corsMethods        = ["OPTIONS", "GET", "PUT", "POST"]
  }
