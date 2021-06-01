module Lib
    ( main
    ) where

import qualified Adapter.InMemory.Auth as M
import qualified Adapter.PostgreSQL.Auth as PG
import qualified Adapter.Redis.Auth as R
import qualified Adapter.RabbitMQ.Common as MQ
import qualified Adapter.RabbitMQ.Auth as MQAuth
import Control.Monad.Reader
import Control.Concurrent.STM (TVar, newTVarIO)
import Domain.Auth
import Katip
import Control.Exception.Safe
import Data.Either (fromRight)
import System.IO (stdout)

type State = (TVar M.State, R.State, PG.State, MQ.State)

newtype App a = App {unApp :: ReaderT State (KatipContextT IO) a} deriving ( Applicative, Functor, Monad
                                                                           , MonadReader State, MonadIO, MonadFail
                                                                           , MonadThrow, KatipContext, Katip, MonadCatch
                                                                           )

instance AuthRepo App where
  addAuth = PG.addAuth
  setEmailVerified = PG.setEmailVerified
  findUserByAuth = PG.findUserByAuth
  findEmailFromUserID = PG.findEmailFromUserID

instance SessionRepo App where
    newSession = R.newSession
    findUserBySessionID = R.findUserBySessionID

instance EmailVerificationNotifier App where
    notifyVerification = MQAuth.notifyVerification


-- An Action To Run
application :: App ()
application = do
    let email = "jmbillson@outlook.com"
    let password = "12fSfs"
    _ <- register email password
    vCode <- pollNotifications email
    _ <- verify vCode
    Right sessionID <- login email password
    Just userID <- resolveSessionID sessionID
    Just registeredEmail <- getUserEmail userID
    liftIO $ print (sessionID, userID, registeredEmail)
    where pollNotifications email = do
              M.getNotificationsForEmail (fromRight undefined $ mkEmail email) >>= \case
                  Just vCode -> return vCode
                  Nothing -> pollNotifications email
              


main :: IO ()
main = withState $ \le state@(_, _, _, mqState) -> do
    let runner = run le state
    MQAuth.initMQ mqState runner
    runner application


-- Takes A Log Environment, An Initial State, And An App To Run And Returns The Result
run :: LogEnv -> State -> App a -> IO a
run le state app = runKatipContextT le () mempty $ runReaderT (unApp app) state


-- Like Run But Creates And Destroys A Katip Log Environment
runWithKatip :: (LogEnv -> IO a) -> IO a
runWithKatip =
    bracket createLogEnv closeScribes
    where createLogEnv = do logEnv <- initLogEnv "HAuth" "prod"
                            stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
                            registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv


withState :: (LogEnv -> State -> IO ()) -> IO ()
withState action = do
    mState <- newTVarIO M.initialState
    runWithKatip $ \le ->
        R.withState $ \reddisConn ->
            PG.withState PG.defaultConfig $ \pool ->
                MQ.withState "Hauth" 16 $ \mqState ->
                    action le (mState, reddisConn, pool, mqState)