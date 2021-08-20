module Adapter.Redis.Auth ( State
                          , withState
                          , newSession
                          , findUserBySessionID
                          ) where

import qualified Database.Redis as R
import qualified Domain.Auth as D
import Data.String (fromString)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (unpack)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Exception.Safe
import Data.Has
import Text.StringRandom

type State = R.Connection

type Redis r m = (Has State r, MonadReader r m, MonadIO m, MonadThrow m)


-- |Given A Function Which Requires A Redis Connection, Initializes The Connection And Passes It In.
withState :: (R.Connection -> IO a) -> IO a
withState = R.withCheckedConnect connectInfo
    where connectInfo = R.defaultConnectInfo


-- |Create New Session For The Given User ID
newSession :: (Redis r m) => D.UserID -> m D.SessionID
newSession userID = do
    sessionID <- liftIO $ stringRandomIO "\\w{20}"
    result <- withConn $ R.set (encodeUtf8 sessionID) (fromString . show $ userID)
    case result of
        Right R.Ok -> withConn (R.expire (encodeUtf8 sessionID) 900) >> return sessionID
        err -> throwString $ "Reddis Error: " <> show err


-- |Given A Session ID, Return The Associated User ID If It Exists
findUserBySessionID :: Redis r m => D.SessionID -> m (Maybe D.UserID)
findUserBySessionID sessionID = do
    result <- withConn $ R.get (encodeUtf8 sessionID)
    case result of
        Right r -> return $ read . unpack . decodeUtf8 <$> r
        err -> throwString $ "Reddis Error: " <> show err


-----------------------------------------------------
------------------ HELPER FUNCTIONS -----------------
-----------------------------------------------------


-- |Given A Redis Contect To Run, Pass In A Connection From The Reader Environment And Return The Result
withConn :: Redis r m => R.Redis a -> m a
withConn action = do
    conn <- asks getter
    liftIO $ R.runRedis conn action
