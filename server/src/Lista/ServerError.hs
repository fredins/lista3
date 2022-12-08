{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lista.ServerError
  ( unauthorized
  , unauthorizedErr
  , forbidden
  , forbiddenErr
  , notFound
  , notFoundErr
  , preconditionFailed
  , preconditionFailedErr
  , serverError
  , serverErrorErr
  )
where

import           Control.Monad.Except        (MonadError)
import qualified Data.ByteString.Lazy        as LBS
import           Relude
import           Servant
import           Text.Blaze                  (ToMarkup (..))
import qualified Text.Blaze.Html             as H
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5.Attributes as HA
import           Text.Blaze.Renderer.Utf8    (renderMarkup)

data Err = Err { errTitle :: Text
               , errMsg   :: Text }

instance ToMarkup Err where
  toMarkup Err{..} = H.docTypeHtml $ do
    H.head $ do
      H.title "Error"
    H.body $ do
      H.h1 (H.a ! HA.href "/" $ "Home")
      H.h2 (H.toHtml errTitle)
      H.p (H.toHtml errMsg)

format :: ToMarkup a => a -> LBS.ByteString
format err = renderMarkup $ toMarkup err

appToErr :: ServerError -> Text -> ServerError
appToErr x msg = x
  { errBody = format (Err (toText (errReasonPhrase x)) msg)
  , errHeaders =  [("Content-Type","text/html")]}

unauthorized :: (MonadError ServerError m) => Text -> m a
unauthorized = throwError . unauthorizedErr

unauthorizedErr :: Text -> ServerError
unauthorizedErr = appToErr err401

forbidden :: (MonadError ServerError m) => Text -> m a
forbidden = throwError . forbiddenErr

forbiddenErr :: Text -> ServerError
forbiddenErr = appToErr err403

notFound :: ( MonadError ServerError m) => Text -> m a
notFound = throwError . notFoundErr

notFoundErr :: Text -> ServerError
notFoundErr = appToErr err404

preconditionFailed :: ( MonadError ServerError m) => Text -> m a
preconditionFailed = throwError . preconditionFailedErr

preconditionFailedErr :: Text -> ServerError
preconditionFailedErr = appToErr err412

serverError :: ( MonadError ServerError m) => Text -> m a
serverError = throwError . serverErrorErr

serverErrorErr :: Text -> ServerError
serverErrorErr = appToErr err500

