module Domain.Auth (
                   -- Types
                     Email
                   , Password
                   , Auth(..)
                   , UserID
                   , VerificationCode
                   , SessionID
                   , RegistrationError(..)
                   , LoginError(..)
                   , mkPassword 
                   , mkEmail
                   , fromEmail
                   , fromPassword
                   , fromAuth

                   -- Ports
                   , AuthRepo(..)
                   , SessionRepo(..)
                   , EmailVerificationNotifier(..)

                   -- Use Cases
                   , register
                   , verify
                   , login
                   , resolveSessionID
                   , getUserEmail

                   -- Test Data
                   , dummyEmail
                   , dummyPassword
                   , dummyAuth
                   ) where
  
import qualified Data.Text as Txt
import Katip.Monadic
import Katip.Core
import Control.Monad.Except
import Domain.Validation

newtype Email = Email Txt.Text deriving (Show, Eq, Ord)

newtype Password = Password Txt.Text deriving (Show, Eq, Ord)

data Auth = Auth { authEmail :: Email, authPassword :: Password } deriving (Show, Eq, Ord)

data RegistrationError = InvalidAuth [String] | AuthNotUniqueError | EmailVerificationError deriving (Show, Eq)

data LoginError = LoginErrorInvalidAuth | LoginErrorEmailNotVerified deriving (Show, Eq)

-- Type Aliases
type VerificationCode = Txt.Text
type EmailText = Txt.Text
type PasswordText = Txt.Text
type UserID = Int
type SessionID = Txt.Text

-- |Types Implementing This Class Provide Access Registering And Retrieving Users Based On Authentication
class (MonadIO m) => AuthRepo m where
    -- |Given An Auth, Add A New User To The System
    addAuth :: Auth -> m (Either RegistrationError (UserID, VerificationCode))
    -- |Verify The User Matching The Given Verification Code
    setEmailVerified :: VerificationCode -> m (Either RegistrationError (UserID, Email))
    -- |Given An Auth, Return The User ID And Their Verification Status If It Exists
    findUserByAuth :: Auth -> m (Maybe (UserID, Bool))
    -- |Find The Email Matching The Given User ID If It Exists
    findEmailFromUserID :: UserID -> m (Maybe Email)

-- |Types Implementing This Class Provide Session Data For An Authenticated User
class (MonadIO m) => SessionRepo m where
    -- |Create New Session For The Given User ID
    newSession :: UserID -> m SessionID
    -- |Given A Session ID, Return The Associated User ID If It Exists
    findUserBySessionID :: SessionID -> m (Maybe UserID)

-- |Types Implementing This Class Provide Email Verification Functionality For Registering New Users
class (MonadIO m) => EmailVerificationNotifier m where
    -- |Send A Verification Code To The Provided Email
    notifyVerification :: Email -> VerificationCode -> m ()
    

withUserIDContext :: (KatipContext m) => UserID -> m a -> m a
withUserIDContext userID = katipAddContext (sl "UserID" userID)


-- |Given An Auth Object, Attempt To Register A New User And Issue A Notification Email. If Authorization Should
-- Fail, Returns A Registration Error.
register :: (KatipContext m, AuthRepo m, EmailVerificationNotifier m) => EmailText -> PasswordText -> m (Either RegistrationError ())
register email password = runExceptT $ do
    auth <- ExceptT (return $ registrationAuthConstructor email password)
    (userID, vCode) <- ExceptT $ addAuth auth
    lift $ notifyVerification (authEmail auth) vCode
    withUserIDContext userID $
        $(logTM) InfoS (ls $ email <> " is registered successfully")


-- |Given A Verification Code, Mark The Corresponding User Profile As Verified
verify :: (AuthRepo m, KatipContext m) => VerificationCode -> m (Either RegistrationError ())
verify verificationCode = runExceptT $ do
    (userID, email) <- ExceptT $ setEmailVerified verificationCode
    withUserIDContext userID $
        $(logTM) InfoS $ ls (fromEmail email) <> " is verified successfully"
    return ()


-- |Given An Email And Password, Log A User Into The App And Return A New Session ID
login :: (KatipContext m, AuthRepo m, SessionRepo m) => EmailText -> PasswordText -> m (Either LoginError SessionID)
login email password = runExceptT $ do
    auth <- ExceptT $ return (loginAuthConstructor email password)
    lift (findUserByAuth auth) >>= \case
        Nothing -> throwError LoginErrorInvalidAuth
        Just (_, False) -> throwError LoginErrorEmailNotVerified
        Just (userID, True) -> lift . withUserIDContext userID $ do 
            session <- newSession userID
            $(logTM) InfoS $ ls email <> " logged in successfully"
            return session


-- |Given A Session ID, Return The Associated User ID If It Exists
resolveSessionID :: (SessionRepo m) => SessionID -> m (Maybe UserID)
resolveSessionID = findUserBySessionID


-- |Given A User ID, Return The User's Email
getUserEmail :: (AuthRepo m) => UserID -> m (Maybe Email)
getUserEmail = findEmailFromUserID


-- |Validates That A Password Is The Correct Length, Contains An Uppercase Letter, Contains A Lowercase Letter,
-- And Contains A Number. Constructs a Password Object On Success & Returns A List Of Errors Upon Failure.
mkPassword :: Txt.Text -> Either [PasswordValidationError] Password
mkPassword = validate Password passwordValidations


-- |A Constructor which Validates That An Email Is Of The Correct Format And Returns An Email Object In Lowercase 
-- If Successful Or A List Of Errors Upon Failure.
mkEmail :: Txt.Text -> Either [EmailValidationError] Email
mkEmail = validate (Email . Txt.toLower) emailValidations


-- |Extract The Inner Text From An Email
fromEmail :: Email -> Txt.Text
fromEmail (Email contents) = contents


-- |Extract The Inner Text From A Password
fromPassword :: Password -> Txt.Text
fromPassword (Password contents) = contents


-- |Extract The Email And Password From An Auth Object
fromAuth :: Auth -> (Email, Password)
fromAuth (Auth email password) = (email, password)


-- |Given An Email And A Password, Attempts To Construct An Auth Object. If Either The Email Or Password Should Fail To
-- Pass Validation, An InvalidAuth Error Will Be Returned Containing A List Of Error Message Strings.
registrationAuthConstructor :: EmailText -> PasswordText -> Either RegistrationError Auth
registrationAuthConstructor email password =
    case (mkEmail email, mkPassword password) of
        (Left xs, Left ys) -> Left $ InvalidAuth (map show xs ++ map show ys)
        (Left xs, Right _) -> Left $ InvalidAuth (map show xs)
        (Right _, Left xs) -> Left $ InvalidAuth (map show xs)
        (Right e, Right p) -> Right $ Auth e p


-- |Given An Email And A Password, Attempts To Construct An Auth Object. If Either The Email Or Password Should Fail To
-- Pass Validation, A LoginErrorInvalidAuth Error Will Be Returned.
loginAuthConstructor :: EmailText -> PasswordText -> Either LoginError Auth
loginAuthConstructor email password =
    case registrationAuthConstructor email password of
        (Left _) -> Left LoginErrorInvalidAuth
        (Right auth) -> Right auth


-- Test Data
dummyAuth :: Auth
dummyAuth = Auth dummyEmail dummyPassword
dummyEmail :: Email
dummyEmail = Email "test@gmail.com"
dummyPassword :: Password
dummyPassword = Password "123456"