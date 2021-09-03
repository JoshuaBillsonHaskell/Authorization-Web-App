module Adapter.HTTP.Web.Main ( main ) where

import Domain.Auth
import Control.Monad.IO.Class
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Katip
import Control.Monad.Cont (lift)

import qualified Network.Wai.Middleware.Static as Wai
import qualified Network.Wai.Middleware.Gzip as Wai
import qualified Network.Wai as Wai
import qualified Data.Text.Lazy as LT
import qualified Adapter.HTTP.Web.Auth as Auth

main :: (MonadIO m, KatipContext m, AuthRepo m, EmailVerificationNotifier m, SessionRepo m) => (m Wai.Response -> IO Wai.Response) -> IO Wai.Application
main runner = do 
    cachingContainer <- Wai.initCaching Wai.PublicStaticCaching
    scottyAppT runner $ routes cachingContainer

routes :: (MonadIO m, KatipContext m, AuthRepo m, EmailVerificationNotifier m, SessionRepo m) => Wai.CacheContainer -> ScottyT LT.Text m ()
routes cachingContainer = do
  
    middleware $ 
        Wai.gzip $ Wai.def { Wai.gzipFiles = Wai.GzipCompress }
    middleware $
        Wai.staticPolicy' cachingContainer (Wai.addBase "src/Adapter/HTTP/Web")
   
    Auth.routes

    notFound $ do
        status status404
        text "Not found"

    defaultHandler $ \e -> do
        lift $ $(logTM) ErrorS $ "Unhandled error: " <> ls (showError e)
        status status500
        text "Internal server error!"