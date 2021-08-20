{-# LANGUAGE DataKinds #-}

module Adapter.HTTP.API.Common where

import Adapter.HTTP.Common
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Domain.Auth
import qualified Data.Text.Lazy as LT

-- |A Wrapper Around getCurrentUserID Which Sets The Response To 401 In The Event That A User ID Cannot Be Found
reqCurrentUserId :: (SessionRepo m, ScottyError e) => ActionT e m UserID
reqCurrentUserId = do
    getCurrentUserId >>= \case
        Nothing -> do
            status status401
            json ("AuthRequired" :: LT.Text)
            finish
        Just userID ->
            return userID
