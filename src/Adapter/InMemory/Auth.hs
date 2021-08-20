module Adapter.InMemory.Auth ( State
                             , InMemory
                             , initialState
                             , addAuth
                             , setEmailVerified
                             , findUserByAuth
                             , findEmailFromUserID
                             , newSession
                             , findUserBySessionID
                             , notifyVerification
                             , getNotificationsForEmail
                             ) where

import qualified Control.Concurrent.STM as STM
import qualified Domain.Auth as D
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Has
import Control.Monad.Reader
import Text.StringRandom (stringRandomIO)

type InMemory r m = (Has (STM.TVar State) r, MonadReader r m, MonadIO m)

-- |This Data Structure Stores The Current State Of The Application
data State = State { stateUsers :: [(D.Auth, D.UserID)]
                   , stateUnverifiedEmails :: M.Map D.VerificationCode D.Email
                   , stateVerifiedEmails :: S.Set D.Email
                   , stateUserIDCount :: D.UserID
                   , stateNotifications :: M.Map D.Email D.VerificationCode
                   , stateSessions :: M.Map D.SessionID D.UserID
                   } deriving (Show)

-- |The Initial State Of Our In Memory Representation Of The Application
initialState :: State
initialState = State { stateUsers = []
                     , stateUnverifiedEmails = mempty
                     , stateVerifiedEmails = mempty
                     , stateUserIDCount = 0
                     , stateNotifications = mempty
                     , stateSessions = mempty
                     }


-- |Given An Auth, Add A New User To The System
addAuth :: InMemory r m => D.Auth -> m (Either D.RegistrationError (D.UserID, D.VerificationCode))
addAuth auth = do
    tVar <- asks getter
    findUserByAuth auth >>= \case
        Nothing -> do
            userID <- liftIO $ stateUserIDCount <$> STM.readTVarIO tVar
            verificationCode <- liftIO $ stringRandomIO "\\w{20}"
            liftIO . STM.atomically $ STM.modifyTVar tVar (addUser auth verificationCode)
            liftIO $ return (Right (userID, verificationCode))
        (Just _) -> do
            liftIO $ return (Left D.AuthNotUniqueError)


-- |Verify The User Matching The Given Verification Code
setEmailVerified :: InMemory r m => D.VerificationCode -> m (Either D.RegistrationError (D.UserID, D.Email))
setEmailVerified verificationCode = do
    tVar <- asks getter
    state <- liftIO $ STM.readTVarIO tVar
    case verifyEmail verificationCode state of
        Nothing -> liftIO . return . Left $ D.EmailVerificationError
        (Just ((userID, email), newState)) -> do
            liftIO $ (STM.atomically . STM.writeTVar tVar $ newState) >> return (Right (userID, email))


-- |Given An Auth, Return The User ID And Their Verification Status If It Exists
findUserByAuth :: InMemory r m => D.Auth -> m (Maybe (D.UserID, Bool))
findUserByAuth auth = do
    tVar <- asks getter
    liftIO (searchForUser auth <$> STM.readTVarIO tVar) >>= \case
        Nothing -> liftIO $ return Nothing
        (Just (_, userID)) -> do
            isVerified <- liftIO $ userIsVerified auth <$> STM.readTVarIO tVar
            liftIO $ return (Just (userID, isVerified))


-- |Find The Email Matching The Given User ID If It Exists
findEmailFromUserID :: InMemory r m => D.UserID -> m (Maybe D.Email)
findEmailFromUserID userID = do
    tVar <- asks getter
    liftIO $ searchForEmail userID <$> STM.readTVarIO tVar


-- |Create New Session For The Given User ID
newSession :: InMemory r m => D.UserID -> m D.SessionID
newSession userID = do
    tvar <- asks getter
    sessionID <- liftIO $ stringRandomIO "\\w{20}"
    findUserBySessionID sessionID >>= \case
        (Just _) -> newSession userID
        Nothing -> do
            liftIO . STM.atomically $ STM.modifyTVar tvar (addNewSession sessionID userID)
            liftIO . return $ sessionID


-- |Given A Session ID, Return The Associated User ID If It Exists
findUserBySessionID :: InMemory r m => D.SessionID -> m (Maybe D.UserID)
findUserBySessionID sessionID = do
    tVar <- asks getter
    liftIO $ M.lookup sessionID . stateSessions <$> STM.readTVarIO tVar


-- |Send A Verification Code To The Provided Email
notifyVerification :: InMemory r m => D.Email -> D.VerificationCode -> m ()
notifyVerification email verificationCode = do
    tVar <- asks getter
    liftIO . STM.atomically $ STM.modifyTVar tVar (sendNotification email verificationCode)


-- Function For Testing Notifications
getNotificationsForEmail :: InMemory r m => D.Email -> m (Maybe D.VerificationCode)
getNotificationsForEmail email = do
    tVar <- asks getter
    liftIO $ M.lookup email . stateNotifications <$> STM.readTVarIO tVar


-- Helper Function
addNewSession :: D.SessionID -> D.UserID -> State -> State
addNewSession sessionID userID state@(State _ _ _ _ _ sessions) =
    state{stateSessions = M.insert sessionID userID sessions}


-- Helper Function For Adding A New User To The State. Takes A New Auth To Add, A Corresponding Verification Code,
-- And The Current State And Returns The New State.
addUser :: D.Auth -> D.VerificationCode -> State -> State
addUser auth verificationCode state@(State users unverifiedEmails _ idCount _ _) =
    state { stateUsers = (auth, idCount):users
          , stateUserIDCount = idCount + 1
          , stateUnverifiedEmails = M.insert verificationCode (fst . D.fromAuth $ auth) unverifiedEmails
          }


-- Helper Function
searchForUser :: D.Auth -> State -> Maybe (D.Auth, D.UserID)
searchForUser auth (State users _ _ _ _ _) =
    case filter ((== auth) . fst) users of
        [] -> Nothing
        (x:_) -> Just x


-- Helper Function
searchForEmail :: D.UserID -> State -> Maybe D.Email
searchForEmail userID (State users _ _ _ _ _) =
    case filter ((== userID) . snd) users of
        [] -> Nothing
        (x:_) -> Just $ (fst . D.fromAuth . fst) x


-- Helper Function
userIsVerified :: D.Auth -> State -> Bool
userIsVerified auth (State _ _ verifiedEmails _ _ _) =
    S.member (fst $ D.fromAuth auth) verifiedEmails


-- Helper Function
verifyEmail :: D.VerificationCode -> State -> Maybe ((D.UserID, D.Email), State)
verifyEmail verificationCode state@(State users unverifiedEmails verifiedEmails _ _ _) = do
    email <- M.lookup verificationCode unverifiedEmails
    (_, userID) <- safeHead $ filter ((==email) . fst . D.fromAuth . fst) users
    return ((userID, email), state { stateUnverifiedEmails = M.delete verificationCode unverifiedEmails
                                   , stateVerifiedEmails = S.insert email verifiedEmails })

-- Helper Function
sendNotification :: D.Email -> D.VerificationCode -> State -> State
sendNotification email verificationCode state@(State _ unverifiedEmails _ _ notifications _) =
    state { stateUnverifiedEmails = M.insert verificationCode email unverifiedEmails
          , stateNotifications = M.insert email verificationCode notifications }


-- Helper Function
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x