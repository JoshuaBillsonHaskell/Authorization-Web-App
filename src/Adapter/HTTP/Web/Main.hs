module Adapter.HTTP.Web.Main ( main ) where

import Domain.Auth
import Control.Monad.IO.Class
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Katip
import qualified Network.Wai as Wai
import qualified Data.Text.Lazy as LT
import Control.Monad.Cont (lift)

main :: (MonadIO m, KatipContext m, AuthRepo m, EmailVerificationNotifier m, SessionRepo m) => (m Wai.Response -> IO Wai.Response) -> IO Wai.Application
main runner = scottyAppT runner routes

routes :: (MonadIO m, KatipContext m, AuthRepo m, EmailVerificationNotifier m, SessionRepo m) => ScottyT LT.Text m ()
routes = do
    get "/" $
        text "Hello From The Web!"

    notFound $ do
        status status404
        text "Not found"

    defaultHandler $ \e -> do
        lift $ $(logTM) ErrorS $ "Unhandled error: " <> ls (showError e)
        status status500
        text "Internal server error!"