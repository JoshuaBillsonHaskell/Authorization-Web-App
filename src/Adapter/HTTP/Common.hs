{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Adapter.HTTP.Common where

import Web.Scotty.Trans
import Control.Monad.Except
import Network.HTTP.Types.Status
import Data.Time
import Domain.Auth
import Blaze.ByteString.Builder (toLazyByteString)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Web.Cookie as Cookie
import qualified Data.Aeson as Aeson
import qualified Web.Forma as Forma
import qualified Data.Text.Lazy as LT

-- Parse JSON From A Request Body And Return The Result Or Return A Status 400 Error If Parsing Fails
parseAndValidateJSON :: (ScottyError e, MonadIO m, Show a, Show e) => Forma.FormParser names e m a -> ActionT e m a
parseAndValidateJSON form = do
    jsonBody <- jsonData
    p <- lift $ Forma.runForm form jsonBody
    case p of
        Forma.Succeeded a -> return a
        _ -> do
            status status400
            text . LT.pack . show $ p
            finish


-- Attempt To Construct An Object From A Given FromJSON Instance And Either Return The Result Or Throw An Error
validator :: (Monad m, Show e) => (s -> Either e a) -> s -> ExceptT LT.Text m a
validator constructor object = do
    case constructor object of
        Left err -> throwError . LT.pack . show $ err
        Right email -> return email


-- Set A Cookie With A Given Name, Value, And Expiration Time In Seconds
setCookie :: (ScottyError e, Monad m) => B.ByteString -> B.ByteString -> DiffTime -> ActionT e m ()
setCookie name value ttl = addHeader "Set-Cookie" cookie
    where cookie = decodeUtf8 . toLazyByteString . Cookie.renderSetCookie $ opts
          opts = Cookie.defaultSetCookie { Cookie.setCookieName = name
                                         , Cookie.setCookieValue = value
                                         , Cookie.setCookieMaxAge = Just ttl
                                         , Cookie.setCookiePath = Just "/"
                                         }


-- Retrieve A Cookie From A Request With A Given Name; Returns None If Matching Cookie Is Not Set
getCookie :: (ScottyError e, Monad m) => B.ByteString -> ActionT e m (Maybe LT.Text)
getCookie key = do
    header "Cookie" >>= \case
        Nothing -> return Nothing
        Just cookiesStr -> do
            let cookie = lookup key . Cookie.parseCookies . LB.toStrict . encodeUtf8 $ cookiesStr
            return $ decodeUtf8 . LB.fromStrict <$> cookie


-- Set The Current Session ID As A Cookie With A Lifespan Of 15 Minutes
setSessionIdInCookie :: (MonadIO m, ScottyError e) => SessionID -> ActionT e m ()
setSessionIdInCookie sessionID = setCookie "SessionID" (TE.encodeUtf8 sessionID) (60 * 15)


getCurrentUserId :: (SessionRepo m, ScottyError e) => ActionT e m (Maybe UserID)
getCurrentUserId = do
    getCookie "SessionID" >>= \case
        Nothing -> return Nothing
        Just sessionID -> lift . findUserBySessionID . LT.toStrict $ sessionID 


reqCurrentUserId :: (SessionRepo m, ScottyError e) => ActionT e m UserID
reqCurrentUserId = do
    getCurrentUserId >>= \case
        Nothing -> do
            status status401
            json ("AuthRequired" :: LT.Text)
            finish
        Just userID -> 
            return userID


foo :: (Monad m, Aeson.FromJSON s) => Forma.FieldName names -> (s -> ExceptT e m a) -> Forma.FormParser names e m a
foo = Forma.field