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

module Lista (module Lista) where

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
import           Relude
import           Servant
import           Servant.HTML.Blaze               (HTML)
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)
import           Web.Cookie                       (SetCookie, def, parseCookies,
                                                   sameSiteLax, sameSiteNone,
                                                   setCookieHttpOnly,
                                                   setCookieName,
                                                   setCookieSameSite,
                                                   setCookieSecure,
                                                   setCookieValue)


oidcConf :: ByteString -> OidcConf
oidcConf password = OidcConf
  { redirectUri = "https://dev.fredin.org/login/cb"
  , clientId = "223213082722-d8vun6oi1alfck9huskmsc1e17l6osd7.apps.googleusercontent.com"
  , clientPassword = password
  }


main :: IO ()
main = do
  pool     <- initConnectionPool defaultConnectInfo
  mgr      <- newManager tlsManagerSettings
  password <- P.head . BC.lines <$> readFileBS "secrets"
  oidcEnv  <- initOidc $ oidcConf password
  withResource pool initDB
  let context' = context pool mgr
      server'  = server  pool mgr oidcEnv

  runTLS tls warp . cors' $ serveWithContext api context' server'

  where
  cors' = (cors . const) $ Just simpleCorsResourcePolicy
    { corsOrigins        = Just (["https://dev1.fredin.org"], True)
    , corsRequestHeaders = ["Content-Type"]
    , corsMethods        = ["OPTIONS", "GET", "PUT", "POST"]
    }
  tls  = tlsSettings "../ssl/cert.pem" "../ssl/key.pem"
  warp = setPort 4000 defaultSettings


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

type Api = Get '[HTML] Homepage
      :<|> OidcApi NoContent
      :<|> "private"      :> AuthProtect "cookie-auth" :> PrivateApi
      :<|> "ping"         :> Get '[PlainText] Text
      :<|> "authenticate" :> QueryParam "sessionKey" Text :> Get '[JSON] (CookieHeader UserDetails)


type CookieHeader = Headers '[Header "Set-Cookie" SetCookie]


server :: Pool Connection -> Manager -> OidcEnv -> Server Api
server pool mgr oidcEnv = pure Homepage
                     :<|> oidcServer oidcEnv (handleSuccessfulLoggedIn pool)
                     :<|> privateServer pool
                     :<|> pure "Pong.\n"
                     :<|> handleAuthenticate pool

handleSuccessfulLoggedIn :: Pool Connection -> AuthInfo -> Handler NoContent
handleSuccessfulLoggedIn pool AuthInfo{..} = do
  sessionKey <- liftIO genRandomBS

  -- Check if new user and act accordingly
  liftIO . withResource pool $ \conn -> do
    -- Get/create user
    user@D.User {id=userId} <- whenNothingM (selectUserById conn sub) $ do
      let user = D.User {id=sub, name=name, email=email}
      insertUser conn user
      pure user

    -- Create session
    whenNothingM_ (selectSession conn (decodeUtf8 sessionKey)) .
      D.insertSession conn $
      D.Session {sessionKey=decodeUtf8 sessionKey, userId=userId}

  redirects $ "https://dev1.fredin.org/auth/" <> sessionKey
  pure NoContent


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
   --    , setCookieHttpOnly = True
       , setCookieSameSite = Just sameSiteNone
       , setCookieSecure   = True
       }

  pure $ addHeader cookie userDetails



type PrivateApi = "userDetails" :> QueryParam "sessionKey" Text :> Get '[JSON] UserDetails
             :<|> "lists"       :> QueryParam "sessionKey" Text :> Get '[JSON] [List]
             :<|> "newList"     :> QueryParam "name" Text :> QueryParam "sessionKey" Text :> Get '[JSON] NoContent
             :<|> "todos"       :> QueryParam "listId" UUID
                                :> QueryParam "active" Bool
                                :> QueryParam "sessionKey" Text
                                :> Get '[JSON] [Todo]
             :<|> "newTodo"     :> ReqBody '[JSON] (UUID -> Todo)
                                :> QueryParam "sessionKey" Text
                                :> Post '[JSON] NoContent
             :<|> "updateTodo"  :> ReqBody '[JSON] Todo
                                :> QueryParam "sessionKey" Text
                                :> Post '[JSON] NoContent


privateServer :: Pool Connection -> Session -> Server PrivateApi
privateServer pool Session{..} = userDetails
                            :<|> lists
                            :<|> newList
                            :<|> todos
                            :<|> newTodo
                            :<|> updateTodo
  where
  userDetails :: Maybe Text -> Handler UserDetails
  userDetails _ = do
    muser <- liftIO $ withResource pool (`selectUserById` userId)
    maybeToRightM (serverErrorErr "No User!") (userToUserDetails <$> muser)

  lists :: Maybe Text -> Handler [List]
  lists _ = liftIO $ withResource pool (`selectAllLists` userId)

  newList :: Maybe Text -> Maybe Text -> Handler NoContent
  newList mname _ = do
    n <- maybeToRightM (preconditionFailedErr "Missing query param name.") mname

    -- Check if list already exists
    lists <- liftIO $ withResource pool (`selectAllLists` userId)
    let name' = name :: List -> Text
    when (any ((==n) . name') lists) $
      preconditionFailed "List already exists!"

    -- Create list and listAccess
    liftIO . withResource pool $ \conn -> do
      listId <- nextRandom
      insertList' conn List {id=listId, name=n}
      insertListAccess conn ListAccess {listId=listId, userId=userId}
    pure NoContent

  todos :: Maybe UUID -> Maybe Bool -> Maybe Text -> Handler [Todo]
  todos mlistId mactive _ = do
    listId <- maybeToRightM (preconditionFailedErr "Missing query param listId.") mlistId
    liftIO . withResource pool $ \conn ->
      case mactive of
        Nothing    -> selectAllTodos conn listId
        Just True  -> selectActiveTodos conn listId
        Just False -> selectCompletedTodos conn listId
  newTodo :: (UUID -> Todo) -> Maybe Text -> Handler NoContent
  newTodo mkTodo _ = do
    liftIO $ withResource pool (`insertTodo` mkTodo)
    pure NoContent

  updateTodo :: Todo -> Maybe Text -> Handler NoContent
  updateTodo todo _ = do
      liftIO $ withResource pool (`D.updateTodo` todo)
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
