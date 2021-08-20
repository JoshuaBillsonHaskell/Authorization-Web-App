module Adapter.PostgreSQL.Auth ( Config(..)
                               , State
                               , defaultConfig
                               , addAuth
                               , setEmailVerified
                               , findUserByAuth
                               , findEmailFromUserID
                               , withPool
                               , withState
                               ) where

import Data.Pool
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple
import Control.Exception.Safe
import Data.Time
import Data.Has
import Text.StringRandom (stringRandomIO)
import Control.Monad.Reader
import Data.Text (Text)
import Data.Maybe (listToMaybe)
import qualified Data.ByteString as B
import qualified Domain.Auth as D

-- |Type Constraints For Our Postgres Environment.
type PG r m = (Has State r, MonadReader r m, MonadIO m, MonadThrow m)

-- |A Pool Of Connections To The Postgres Server
type State = Pool Connection

-- |Configurations For The Postgres Server Connection Pool
data Config = Config { configConnectionInfo :: ConnectInfo
                     , configStripeCount :: Int
                     , configMaxOpenConnPerStrip :: Int
                     , configIdleConnTimeout :: NominalDiffTime
                     }


-- |Default Database Configuration
defaultConfig :: Config
defaultConfig = Config { configConnectionInfo = defaultConnectInfo { connectDatabase = "hauth" }
                       , configStripeCount = 2
                       , configMaxOpenConnPerStrip = 5
                       , configIdleConnTimeout = 10
                       }


-- |Add New User To Database
addAuth :: PG r m => D.Auth -> m (Either D.RegistrationError (D.UserID, D.VerificationCode))
addAuth auth = do
    let (email, password) = D.fromAuth auth
    vCode <- liftIO $ (D.fromEmail email <>) <$> stringRandomIO "_\\w{20}"
    insertNewUser (D.fromEmail email, D.fromPassword password, vCode) >>= \case
        Right [Only uID] -> return $ Right (uID, vCode)
        Right _ -> throwString "Should Not Happen: The Query Doesn't Return A User Id!"
        Left err -> catchInsertUserError err


-- |Verify A User Associated With A Given Verification Code. Return The Corresponding User ID And Email In The Event
-- Of A Success Or Return A RegistrationError In The Event Of Failure.
setEmailVerified :: PG r m => D.VerificationCode -> m (Either D.RegistrationError (D.UserID, D.Email))
setEmailVerified vCode = do
    verifyEmail vCode >>= \case
        [(userID, email)] -> do
            case D.mkEmail email of
                Left _ -> return $ Left D.EmailVerificationError
                Right e -> return $ Right (userID, e)
        _ -> return $ Left D.EmailVerificationError


-- |Given An Authentication Object, Retrieves The Corresponding User ID And Their Verification Status If
-- Available. Returns Nothing If No Matching User Can Be Located.
findUserByAuth :: PG r m => D.Auth -> m (Maybe (D.UserID, Bool))
findUserByAuth auth = listToMaybe <$> result
    where result = withConn $ \conn -> do
            let email = D.fromEmail . fst . D.fromAuth $ auth
            let password = D.fromPassword . snd . D.fromAuth $ auth
            let statement = "SELECT id, is_email_verified FROM auths WHERE email=? AND pass=crypt(?, pass);"
            query conn statement (email, password)


-- |Retrieves A User's Email Associated With The Given User ID If It Exists. Returns Nothing If No Such User Exists.
findEmailFromUserID :: PG r m => D.UserID -> m (Maybe D.Email)
findEmailFromUserID userID = do
    findEmail userID >>= \case
        [Only email] -> do
            case D.mkEmail email of
                Left _ -> return Nothing
                Right e -> return $ Just e
        _ -> return Nothing


-- |Creates A Configured Connection Pool And Runs A Given Action. Destroys All Resources In The Pool Upon Completion.
withPool :: Config -> (State -> IO a) -> IO a
withPool config = bracket (initPool config) destroyAllResources


-- |Like withPool But Migrates The Database Before Performing The Given Action.
withState :: Config -> (State -> IO a) -> IO a
withState config action =
    withPool config $ \state -> do
        migrate state
        action state


------------------------------------------------------
------------------ HELPER FUNCTIONS ------------------
------------------------------------------------------


-- |Helper Function Which Inserts A New User Into The Database
insertNewUser :: PG r m => (Text, Text, Text) -> m (Either SqlError [Only D.UserID])
insertNewUser (email, password, vCode) = withConn $ \conn ->
    try $ query conn statement (email, password, vCode, False)
    where statement = " INSERT INTO auths \
                      \ (email, pass, email_verification_code, is_email_verified) VALUES \
                      \ (?, crypt(?, gen_salt('bf')), ?, ?) returning id; "


-- |Helper Function For Handling Errors Thrown When Inserting A New User Into The Database
catchInsertUserError :: PG r m => SqlError -> m (Either D.RegistrationError (D.UserID, D.VerificationCode))
catchInsertUserError err@SqlError{sqlState = state, sqlErrorMsg = msg} = do
    if state == "23505" && B.isInfixOf "auths_email_key" msg
        then return $ Left D.AuthNotUniqueError
        else throwString $ "Unhandled PostgreSQL Exception: " <> show err


-- |Helper Function Which Takes A Verification Code And Verifies The Corresponding User In The Database
verifyEmail :: PG r m => D.VerificationCode -> m [(Int, Text)]
verifyEmail vCode = withConn $ \conn -> do
    query conn statement $ Only vCode
    where statement = " UPDATE auths \
                      \ SET is_email_verified = true \
                      \ WHERE email_verification_code = ? \
                      \ RETURNING id, cast (email as text); "


-- |Helper Function That Retrieves A User's Email Associated With A Given ID.
findEmail :: PG r m => D.UserID -> m [Only Text]
findEmail userID = withConn $ \conn -> do
    let statement = "SELECT cast (email as TEXT) FROM auths WHERE id = ?;"
    query conn statement (Only userID)


-- |Helper Function For Initilizing A Pool Of Connections
initPool :: Config -> IO State
initPool (Config connectInfo stripeCount connectionsPerStripe connectionTimout) =
    createPool (connect connectInfo) close stripeCount connectionTimout connectionsPerStripe


-- |Helper Function For Database Migration
runMigrationTransaction :: Connection -> IO (MigrationResult String)
runMigrationTransaction conn = withTransaction conn $ runMigrations False conn commands
    where commands = [MigrationInitialization, MigrationDirectory "src/Adapter/PostgreSQL/Migrations"]


-- |Perform Database Migration Inside A Transaction; Throw An Error If The Migration Cannot Be Completed.
migrate :: State -> IO ()
migrate pool = withResource pool $ \conn -> do
    runMigrationTransaction conn >>= \case
        (MigrationError e) -> throwString e
        _ -> return ()


-- |Helper Function Which Performs An Action That Requires A Database Connection.
withConn :: PG r m => (Connection -> IO a) -> m a
withConn action = do
    pool <- asks getter
    liftIO $ withResource pool action