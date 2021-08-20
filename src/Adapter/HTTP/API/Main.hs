module Adapter.HTTP.API.Main where

import Domain.Auth
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Katip
import Control.Monad.IO.Class

import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Gzip as WaiMiddleware
import qualified Adapter.HTTP.API.Auth as AuthAPI
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Control.Monad.Cont (lift)


-- |Runs Our Routes On A Given Port When Supplied With A Runner Function
main :: (MonadIO m, KatipContext m, AuthRepo m, EmailVerificationNotifier m, SessionRepo m) => (m Wai.Response -> IO Wai.Response) -> IO Wai.Application
main runner = scottyAppT runner routes


-- |Defines The Routing And Middleware Of Our App.
routes :: (MonadIO m, Katip.KatipContext m, AuthRepo m, EmailVerificationNotifier m, SessionRepo m) => ScottyT LT.Text m ()
routes = do
    -- Setup Middleware
    middleware (WaiMiddleware.gzip $ WaiMiddleware.def { WaiMiddleware.gzipFiles = WaiMiddleware.GzipCompress })

    -- Add API Routes
    AuthAPI.routes

    notFound $ do
       status status404
       json ("NotFound" :: T.Text)

    -- Override Default Error Handler
    defaultHandler $ \e -> do
        lift $ $(logTM) ErrorS $ "Unhandled error: " <> ls (showError e)
        status status500
        json ("InternalServerError" :: T.Text)
