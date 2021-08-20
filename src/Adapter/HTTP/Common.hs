{-# LANGUAGE DataKinds #-}

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


-- |Set A Cookie With A Given Name, Value, And Expiration Time In Seconds
setCookie :: (Monad m) => B.ByteString -> B.ByteString -> DiffTime -> ActionT e m ()
setCookie name value ttl = addHeader "Set-Cookie" cookie
    where cookie = decodeUtf8 . toLazyByteString . Cookie.renderSetCookie $ opts
          opts = Cookie.defaultSetCookie { Cookie.setCookieName = name
                                         , Cookie.setCookieValue = value
                                         , Cookie.setCookieMaxAge = Just ttl
                                         , Cookie.setCookiePath = Just "/"
                                         }


-- |Retrieve A Cookie From A Request With A Given Name; Returns None If Matching Cookie Is Not Set
getCookie :: (ScottyError e, Monad m) => B.ByteString -> ActionT e m (Maybe LT.Text)
getCookie key = do
    cookiesStr <- header "Cookie"
    return $ do 
        cookies <- Cookie.parseCookies . LB.toStrict . encodeUtf8 <$> cookiesStr
        decodeUtf8 . LB.fromStrict <$> lookup key cookies


-- |Set The Current Session ID As A Cookie With A Lifespan Of 15 Minutes
setSessionIdInCookie :: (MonadIO m) => SessionID -> ActionT e m ()
setSessionIdInCookie sessionID = setCookie "SessionID" (TE.encodeUtf8 sessionID) (60 * 15)


-- |Return The User ID Of The Current Session User
getCurrentUserId :: (SessionRepo m, ScottyError e) => ActionT e m (Maybe UserID)
getCurrentUserId = do
    getCookie "SessionID" >>= \case
        Nothing -> return Nothing
        Just sessionID -> lift . resolveSessionID . LT.toStrict $ sessionID


-- |A Wrapper Around getCurrentUserID Which Sets The Response To 401 In The Event That A User ID Cannot Be Found
reqCurrentUserId :: (SessionRepo m, ScottyError e) => ActionT e m UserID
reqCurrentUserId = do
    getCurrentUserId >>= \case
        Nothing -> do
            status status401
            json ("AuthRequired" :: LT.Text)
            finish
        Just userID ->
            return userID