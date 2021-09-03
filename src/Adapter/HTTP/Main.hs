module Adapter.HTTP.Main where

import Domain.Auth
import Katip
import Network.Wai.Middleware.Vhost

import qualified Network.Wai as Wai
import qualified Adapter.HTTP.API.Main as API
import qualified Adapter.HTTP.Web.Main as Web
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp


-- |Runs Our Application On A Given Port
main :: (KatipContext m, AuthRepo m, EmailVerificationNotifier m, SessionRepo m) => Int -> (m Wai.Response -> IO Wai.Response) -> IO ()
main port runner = do
    web <- Web.main runner
    api <- API.main runner
    Warp.run port $ vhost [(pathBeginsWith "api", api)] web

 
-- |Checks That A Path Begins With A Given String
pathBeginsWith :: T.Text -> Wai.Request -> Bool
pathBeginsWith path = (== Just path) . safeHead . Wai.pathInfo
    where safeHead [] = Nothing
          safeHead (x:_) = Just x