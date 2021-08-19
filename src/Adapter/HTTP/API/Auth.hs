module Adapter.HTTP.API.Auth where

import Web.Scotty.Trans
import Domain.Auth
import Adapter.HTTP.Common
import Network.HTTP.Types.Status
import Control.Monad.IO.Class
import Adapter.HTTP.Validation

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Katip
import Control.Monad.Cont (lift)

-- |Defines The Routes Used By Our Application
routes :: (MonadIO m, Katip.KatipContext m, AuthRepo m, EmailVerificationNotifier m, SessionRepo m) => ScottyT LT.Text m ()
routes = do
    -- Register A New User
    post "/api/auth/register" $ do
        auth <- parseAndValidateJSON authForm
        let (email, pass) = fromAuth auth
        result <- lift $ register (fromEmail email) (fromPassword pass)
        case result of
            Right () ->
                return ()
            Left (InvalidAuth _) -> do
                status status400
                json ("Invalid Authorization" :: T.Text)
            Left AuthNotUniqueError -> do
                status status400
                json ("Email ALready Taken" :: T.Text)
            Left EmailVerificationError -> do
                raise "This Shouldn't Happen! (Registration Attempted To Verify Email)"

    -- Verify An Email
    post "/api/auth/verify" $ do
        vCode <- parseAndValidateJSON verifyEmailForm
        lift (verify (LT.toStrict vCode)) >>= \case
            Right () ->
                return ()
            Left EmailVerificationError -> do
                status status400
                json ("Invalid Verification Code!" :: T.Text)
            Left _ -> do
                raise "This Shouldn't Happen! (Verification Raised Unexpected Error)"

    -- Login A User And Set A Session ID
    post "/api/auth/login" $ do
        auth <- parseAndValidateJSON authForm
        let (email, password) = fromAuth auth
        result <- lift $ login (fromEmail email) (fromPassword password)
        case result of
            Right sessionID -> do
                setSessionIdInCookie sessionID
                return ()
            Left LoginErrorInvalidAuth -> do
                status status400
                json ("Invalid Login Credentials!" :: T.Text)
            Left LoginErrorEmailNotVerified -> do
                status status400
                json ("Email Not Verified!" :: T.Text)

    -- Take The Session ID If Present And Return The Email Corresponding To The Session's User
    get "/api/users" $ do
        userID <- reqCurrentUserId
        lift (getUserEmail userID) >>= \case
            Nothing -> do
                raise "Should not happen: SessionId map to invalid UserId"
            Just email -> do
                json (fromEmail email)