module Adapter.HTTP.Web.Auth where

import Web.Scotty.Trans
import Domain.Auth
import Text.Digestive.Form ((.:))
import Adapter.HTTP.Common
import Adapter.HTTP.Web.Common
import Katip
import Text.Blaze.Html5 ((!))
import Control.Monad.IO.Class
import Control.Monad.Cont (lift)

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.View as DF
import qualified Text.Digestive.Types as DF
import qualified Text.Digestive.Scotty as DF
import qualified Text.Digestive.Blaze.Html5 as DH
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


-- |Routes For Displaying Web Content To The Client
routes :: (MonadIO m, AuthRepo m, SessionRepo m, EmailVerificationNotifier m, KatipContext m) => ScottyT LT.Text m ()
routes = do
    -- Home Screen
    get "/" $
        redirect "/users"

    -- Registration Page
    get "/auth/register" $ do
        view <- DF.getForm "auth" authForm
        renderHtml $ registerPage view []

    -- Registration
    post "/auth/register" $ do

        -- Validate Form
        (view, maybeAuth) <- DF.runForm "auth" authForm
        case maybeAuth of
            Nothing ->
                renderHtml $ registerPage view []

            -- If Form Is Valid, Register The User
            Just auth -> do
                let (email, password) = fromAuth auth
                lift (register (fromEmail email) (fromPassword password)) >>= \case
                    Left AuthNotUniqueError -> do
                        renderHtml $ registerPage view ["Email Has Been Taken"]
                    Right _ -> do
                        v <- DF.getForm "auth" authForm
                        renderHtml $ registerPage v ["Registered Successfully!"]
                    _ -> do
                        raise "Error: Registration Caused An Unexpected Error! (This Should Not Happen)"

    -- Verify Email
    post "/auth/verifyEmail/:code" $ do
        code <- rescue (param "code") (const $ return "")
        lift (setEmailVerified code) >>= \case
            Left EmailVerificationError ->
                renderHtml $ verifyEmailPage "The Verification Code Is Invalid"
            Left (InvalidAuth _) ->
                raise "Error: Email Verification Returned Invalid Auth! (This Should Not Happen)"
            Left AuthNotUniqueError ->
                raise "Error: Email Verification Returned Auth Not Unique! (This Should Not Happen)"
            Right _ ->
                renderHtml $ verifyEmailPage "Your Email Has Been Verified"

    -- Login Page
    get "/auth/login" $ do
        view <- DF.getForm "auth" authForm
        renderHtml $ loginPage view []

    -- Login
    post "/auth/login" $ do

        -- Validate Form
        (view, maybeAuth) <- DF.runForm "auth" authForm
        case maybeAuth of
            Nothing ->
                renderHtml $ loginPage view []

            -- If Form Is Valid, Login The User
            Just auth -> do
                let (email, password) = fromAuth auth
                lift (login (fromEmail email) (fromPassword password)) >>= \case
                    Left LoginErrorInvalidAuth -> do
                        renderHtml $ loginPage view ["Email And/Or Password Is Not Correct!"]
                    Left LoginErrorEmailNotVerified -> do
                        renderHtml $ loginPage view ["Email Has Not Been Verified!"]
                    Right sessionID -> do
                        setSessionIdInCookie sessionID
                        redirect "/"

    -- User Page
    get "/users" $ do
        userID <- reqCurrentUserID
        lift (getUserEmail userID) >>= \case
            Nothing ->
                raise "Error: Email Not Found!"
            Just email ->
                renderHtml $ userPage (fromEmail email)


-- |HTML Layout For The User Page
userPage :: T.Text -> H.Html
userPage email =
    mainLayout "Users" $ do
        H.div $
            H.h1 "Users"
        H.div $
            H.toHtml email


-- |HTML Layout For The Verify Email Page
verifyEmailPage :: T.Text -> H.Html
verifyEmailPage msg =
    mainLayout "Verify Email" $ do
        H.h1 "Email Verification"
        H.div $ H.toHtml msg
        H.div $ H.a ! A.href "/auth/login" $ "Login"


-- |HTML Layout For The Registration Page
registerPage :: DF.View [T.Text] -> [T.Text] -> H.Html
registerPage view msgs =
    mainLayout "Register" $ do
        H.div $
            authFormLayout view "Register" "/auth/register" msgs
        H.div ! A.class_ "py-3" $
            H.a ! A.href "/auth/login" $ "Login"


-- |HTML Layout For The Login Page
loginPage :: DF.View [T.Text] -> [T.Text] -> H.Html
loginPage view msgs =
    mainLayout "Login" $ do
        H.div $
            authFormLayout view "Login" "/auth/login" msgs
        H.div ! A.class_ "py-3" $
            H.a ! A.href "/auth/register" $ "Register"


-- |HTML Layout For The Login/Registration Form
authFormLayout :: DF.View [T.Text] -> T.Text -> T.Text -> [T.Text] -> H.Html
authFormLayout view formTitle action msgs =
    formLayout view action $ do
        H.h2 $ H.toHtml formTitle
        H.div $ errorList msgs
        H.div $ do
            DH.inputText "email" view ! A.placeholder "Email"
            H.div $ errorList' "email"
        H.div $ do
            DH.inputPassword "password" view ! A.placeholder "Password"
            H.div $ errorList' "password"
        H.input ! A.type_ "submit" ! A.value "Submit" ! A.class_ "btn btn-outline-dark btn-lg"
    where errorList items = H.ul ! A.style "list-style-type:none;" ! A.class_ "px-0" $ mapM_ errorItem items
          errorList' path = errorList $ mconcat (DF.errors path view)
          errorItem = H.li . H.toHtml


-- |Digestive Functor Validation For An Auth Form
authForm :: (Monad m) => DF.Form [T.Text] m Auth
authForm = Auth <$> ("email" .: emailForm) <*> ("password" .: passwordForm)


-- |Digestive Functor Validation For An Email Form
emailForm :: (Monad m) => DF.Form [T.Text] m Email
emailForm = DF.validate (toResult . mkEmail) (DF.text Nothing)


-- |Digestive Functor Validation For A Password Form
passwordForm :: (Monad m) => DF.Form [T.Text] m Password
passwordForm = DF.validate (toResult . mkPassword) (DF.text Nothing)


-- |Converts The Output Of A Constructor Which Returns Either The Result Or An Error Into A Digestive Functor Result
toResult :: (Show e) => Either [e] a -> DF.Result [T.Text] a
toResult = either (DF.Error . map (T.pack . show)) DF.Success