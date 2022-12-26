{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedLists          #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PartialTypeSignatures    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}

module Lista.Oidc
  ( oidcServer
  , OidcConf(..)
  , OidcApi
  , AuthInfo(..)
  , OidcEnv
  , Homepage(Homepage)
  , initOidc
  , genRandomBS
  , redirects
  , SuccessPage (SuccessPage)
  )
where

import           Control.Monad.Except        (MonadError, liftEither)
import           Data.Aeson                  (FromJSON (..), (.:))
import qualified Data.Aeson                  as JSON
import qualified Data.Aeson.Types            as AeT
import           Data.List                   ((!!))
import           Jose.Jwt                    (Jwt (..), decodeClaims)
import           Network.HTTP.Client         (Manager, newManager)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Relude
import           Servant
import           Servant.HTML.Blaze          (HTML)
import qualified System.Random               as Random
import           Text.Blaze                  (ToMarkup (..))
import qualified Text.Blaze.Html             as H
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Web.OIDC.Client             as O

import           Data.Either.Extra           (mapLeft)
import           Lista.Auxiliary
import           Lista.ServerError

type OidcApi a =
  "login" :> (
              Get '[JSON] NoContent
              :<|> "cb" :> QueryParam "error" Text
                        :> QueryParam "code" Text
                        :> Get '[HTML] a)

data OidcEnv = OidcEnv
  { oidc           :: O.OIDC
  , mgr            :: Manager
  , genState       :: IO ByteString
  , prov           :: O.Provider
  , redirectUri    :: ByteString
  , clientId       :: ByteString
  , clientPassword :: ByteString
  }

oidcServer :: OidcEnv -> (AuthInfo -> Handler a) -> Server (OidcApi a)
oidcServer oidc handler = handleLogin oidc
                     :<|> handleLoggedIn oidc handler

-- | Redirect user to the OpenID Provider
handleLogin :: OidcEnv -> Handler NoContent
handleLogin oidcenv = do
  loc <- liftIO (genOidcUrl oidcenv)
  redirects loc
  pure NoContent

-- | Check and process the OpenId response. Return the @handleSuccess@ handler as response.
handleLoggedIn :: OidcEnv
               -> (AuthInfo -> Handler a)
               -> Maybe Text -- ^ error
               -> Maybe Text -- ^ code
               -> Handler a
handleLoggedIn oidcenv handleSuccess err mcode = do
  -- Check no error
  maybeToLeftM_ (forbiddenErr <$> err)
  -- Check code
  code <- maybeToRightM (forbiddenErr "No code parameter given") mcode
  -- Request tokens
  (tokens :: O.Tokens AuthInfo) <- liftIO $ O.requestTokens (oidc oidcenv) Nothing (encodeUtf8 code) (mgr oidcenv)
  let jwt = unJwt . O.idTokenJwt $ tokens
      err = forbiddenErr . (<>) "JWT decode/check problem: " . show
  -- Decode claims
  (_, authInfo) <- liftEither . mapLeft err $ decodeClaims jwt

  if emailVerified authInfo
    then handleSuccess authInfo
    else forbidden "Please verify your email"

data OidcConf = OidcConf
  { redirectUri    :: ByteString
  , clientId       :: ByteString
  , clientPassword :: ByteString
  } deriving (Show, Eq)

initOidc :: OidcConf -> IO OidcEnv
initOidc OidcConf{..} = do
  mgr  <- newManager tlsManagerSettings
  prov <- O.discover "https://accounts.google.com" mgr
  let oidc = O.setCredentials clientId clientPassword redirectUri (O.newOIDC prov)
  pure OidcEnv { oidc = oidc
               , mgr = mgr
               , genState = genRandomBS
               , prov = prov
               , redirectUri = redirectUri
               , clientId = clientId
               , clientPassword = clientPassword
               }

-- | gen a 302 redirect helper
redirects :: MonadError ServerError m => ByteString -> m ()
redirects url = throwError err302 { errHeaders = [("Location", url)]}

genOidcUrl :: OidcEnv -> IO ByteString
genOidcUrl OidcEnv{..} = do
  st <- genState -- generate a random string
  let oidcCreds = O.setCredentials clientId clientPassword redirectUri (O.newOIDC prov)
  loc <- O.getAuthenticationRequestUrl oidcCreds [O.openId, O.email, O.profile] (Just st) []
  return (show loc)

-- | generate a random ByteString, not necessarily extremely good randomness
-- still the password will be long enough to be very difficult to crack
genRandomBS :: IO ByteString
genRandomBS = do
  g <- Random.newStdGen
  Random.randomRs (0, n) g & take 42 & fmap toChar & readable 0 & encodeUtf8 & return
  where
    n = length letters - 1
    toChar i = letters !! i
    letters = ['A'..'Z'] <> ['0'..'9'] <> ['a'..'z']
    readable :: Int -> [Char] -> [Char]
    readable _ [] = []
    readable i str =
      let blocksize = case n of
            0 -> 8
            1 -> 4
            2 -> 4
            3 -> 4
            _ -> 12
          block = take blocksize str
          rest = drop blocksize str
      in if null rest
         then str
         else block <> "-" <> readable (i+1) rest



data AuthInfo = AuthInfo
  { email         :: Text
  , emailVerified :: Bool
  , name          :: Text
  , sub           :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON AuthInfo where
  parseJSON (JSON.Object v) = do
    email          <- v .: "email"
    email_verified <- v .: "email_verified"
    name           <- v .: "name"
    sub            <- v .: "sub"
    return $ AuthInfo email email_verified name sub
  parseJSON invalid    = AeT.typeMismatch "Coord" invalid

instance JSON.ToJSON AuthInfo where
  toJSON (AuthInfo e ev n s) =
    JSON.object [ "email"          JSON..= e
                , "email_verified" JSON..= ev
                , "name"           JSON..= n
                , "sub"            JSON..= s
                ]

newtype SuccessPage = SuccessPage Text

instance ToMarkup SuccessPage where
  toMarkup (SuccessPage sessionKey) = H.docTypeHtml $ do
    H.h1 $ H.toHtml ("Success: " <> sessionKey)
    H.script $ H.toHtml ("window.opener.postMessage('" <> sessionKey <> "', '*')")
    (H.button ! HA.onclick "window.close();") $ H.toHtml ("Close" :: Text)

data Failure = Failure
instance ToMarkup Failure where
  toMarkup Failure = H.docTypeHtml $ do
    H.h1 $ H.toHtml ("Login Failed" :: Text)
    (H.button ! HA.onclick "window.close();") $ H.toHtml ("Close" :: Text)

data Homepage = Homepage

instance ToMarkup Homepage where
  toMarkup Homepage = H.docTypeHtml $ do
    H.head $ do
      H.title "OpenID Connect Servant Example"
      H.style (H.toHtml ("body { font-family: monospace; font-size: 18px; }" :: Text))
    H.body $ do
      H.h1 "OpenID Connect Servant Example"
      H.div $
        H.a ! HA.href "/server/login" $ "Click here to login"
      H.ul $ do
        H.li $ do
          H.span "API Key in Local storage: "
          H.script (H.toHtml ("document.write(localStorage.getItem('api-key'));" :: Text))
        H.li $ do
          H.span "User ID in Local storage: "
          H.script (H.toHtml ("document.write(localStorage.getItem('user-id'));" :: Text))


