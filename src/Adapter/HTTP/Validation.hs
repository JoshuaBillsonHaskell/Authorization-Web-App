{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Adapter.HTTP.Validation ( parseAndValidateJSON 
                               , authForm 
                               , verifyEmailForm
                               ) where

import Web.Scotty.Trans
import Control.Monad.Except
import Network.HTTP.Types.Status
import Data.Time
import Domain.Auth
import Blaze.ByteString.Builder (toLazyByteString)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Web.Cookie as Cookie
import qualified Data.Aeson as Aeson
import qualified Web.Forma as Forma
import qualified Data.Text.Lazy as LT


-- Parse JSON From A Request Body And Return The Result Or Return A Status 400 Error If Parsing Fails
parseAndValidateJSON :: (ScottyError e, MonadIO m, Show a, Show e) => Forma.FormParser names e m a -> ActionT e m a
parseAndValidateJSON form = do
    jsonBody <- jsonData `rescue` (\_ -> return Aeson.Null)
    p <- lift $ Forma.runForm form jsonBody
    case p of
        Forma.Succeeded a -> return a
        _ -> do
            status status400
            text . LT.pack . show $ p
            finish


-- Attempt To Construct An Object From A Given FromJSON Instance And Either Return The Result Or Throw An Error
validator :: (Monad m, Show e, Aeson.FromJSON s) => (s -> Either e a) -> s -> ExceptT LT.Text m a
validator constructor object = do
    case constructor object of
        Left err -> throwError . LT.pack . show $ err
        Right email -> return email


notEmpty :: (Monad m) => LT.Text -> ExceptT LT.Text m LT.Text
notEmpty field = 
    if LT.null field
        then throwError "This Field Cannot Be Empty!"
        else return field


authForm :: (Monad m) => Forma.FormParser '["email", "password"] LT.Text m Auth
authForm = Auth <$> Forma.field #email emailValidator <*> Forma.field #password passwordValidator
    where emailValidator = validator mkEmail
          passwordValidator = validator mkPassword


verifyEmailForm :: (Monad m) => Forma.FormParser '["verificationCode"] LT.Text m LT.Text
verifyEmailForm = Forma.field #verificationCode notEmpty