module Adapter.HTTP.Web.Common where

import Web.Scotty.Trans
import Domain.Auth
import Text.Blaze.Html5 ((!))
import Adapter.HTTP.Common

import qualified Data.Text as T
import qualified Text.Digestive.View as DF
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as H


-- |Render HTML To The Client
renderHtml :: (ScottyError e, Monad m) => H.Html -> ActionT e m ()
renderHtml = html . H.renderHtml


-- |HTML Template From Which All Other Pages Inherit
mainLayout :: T.Text -> H.Html -> H.Html
mainLayout title content =
    H.docTypeHtml $ do
        H.head $ do
            favicon "/static/images/favicon.png"
            bootstrap
            H.title $ H.toHtml title
        H.body $ do
            H.div ! A.class_ "container-fluid" $ do
                H.div ! A.class_ "row align-content-center text-center" $ do
                    H.div ! A.class_ "col-12" $ 
                        H.div $ H.img ! A.src "/static/images/logo.png" ! A.class_ "my-4"
                    H.div ! A.class_ "col-12" $ 
                        H.div content


-- |HTML Layout For Setting The Favicon
favicon :: H.AttributeValue -> H.Html
favicon path = H.link ! A.rel "icon" ! A.type_ "image/png" ! A.href path

bootstrap :: H.Html
bootstrap = H.link ! A.rel "stylesheet" ! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"

-- |HTML Layout For A Form
formLayout :: DF.View a -> T.Text -> H.Html -> H.Html
formLayout view action =
    H.form ! A.method "POST"
           ! A.enctype (H.toValue . show . DF.viewEncType $ view)
           ! A.action (H.toValue action)


-- |Request The Current User ID; Redirects The CLient To The Login Page In The Event They Are Not Already Logged In
reqCurrentUserID :: (SessionRepo m, ScottyError e) => ActionT e m UserID
reqCurrentUserID = do
    getCurrentUserId >>= \case
        Nothing ->
            redirect "/auth/login"
        Just userID ->
            return userID