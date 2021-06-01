module Adapter.RabbitMQ.Auth where

import qualified Adapter.InMemory.Auth as M
import qualified Network.AMQP as AMQP
import qualified Data.Text as T
import Katip
import Control.Exception.Safe
import Adapter.RabbitMQ.Common
import Data.Aeson
import Domain.Auth as D
import Control.Monad.Reader

data EmailVerificationPayload = EmailVerificationPayload
                                { emailVerificationPayloadEmail :: T.Text
                                , emailVerificationPayloadVerificationCode :: T.Text
                                }

instance FromJSON EmailVerificationPayload where
    parseJSON = withObject "EmailVerificationPayload" $ \v ->
        EmailVerificationPayload <$> v .: "email"  <*> v .: "verificationCode"

instance ToJSON EmailVerificationPayload where
    toJSON (EmailVerificationPayload email vCode) = object ["email" .= email, "verificationCode" .= vCode]
    toEncoding (EmailVerificationPayload email vCode) = pairs $ "email" .= email <> "verificationCode" .= vCode


-- Publish JSON Payload Containing Email Verification Details To RabbitMQ
notifyVerification :: Rabbit r m => D.Email -> D.VerificationCode -> m ()
notifyVerification email vCode = do
    let payload = EmailVerificationPayload (D.fromEmail email) vCode
    publish "auth" "registration" payload


-- RabbitMQ Handler For Sending Email Verification
consumeEmailVerification :: (M.InMemory r m, KatipContext m, MonadCatch m) => (m Bool -> IO Bool) -> (AMQP.Message, AMQP.Envelope) -> IO Bool
consumeEmailVerification runner (msg, _) = 
    runner $ consumeAndProcess msg handler
    where handler (EmailVerificationPayload email vCode) = 
              case D.mkEmail email of
                  Left _ -> do
                      $(logTM) ErrorS "Invalid Email Format!"
                      return False
                  Right email' -> do
                      M.notifyVerification email' vCode
                      return True


-- Initialize RabbitMQ Topology
initMQ :: (M.InMemory r m, KatipContext m, MonadCatch m) => State -> (m Bool -> IO Bool) -> IO ()
initMQ state runner = do
    void $ initQueue state "verifyEmail" "auth" "registration"
    initConsumer state "verifyEmail" (consumeEmailVerification runner)