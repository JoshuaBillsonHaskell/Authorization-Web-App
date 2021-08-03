module Adapter.HTTP.API.Auth where

import Web.Scotty.Trans
import Domain.Auth
import Adapter.HTTP.Common
import Network.HTTP.Types.Status
import Control.Monad.IO.Class

import qualified Web.Forma as Forma
import qualified Data.Aeson as Aeson
import qualified Katip

routes :: (ScottyError e, MonadIO m, Katip.KatipContext m, AuthRepo m, EmailVerificationNotifier m, SessionRepo m) => ScottyT e m ()
routes = do
    -- Register
    post "/api/auth/register" undefined

    -- Verify Email
    post "/api/auth/verify" undefined

    -- Login
    post "/api/auth/login" undefined

    -- Get User
    get "/api/users" undefined
