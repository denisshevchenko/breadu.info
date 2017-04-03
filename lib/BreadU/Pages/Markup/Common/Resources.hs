{-|
Module      : BreadU.Pages.Markup.Common.Resources
Description : Resources for the pages.
Stability   : experimental
Portability : POSIX

Resources for the pages (styles, fonts, scripts), including external ones (from the CDN).
-}

module BreadU.Pages.Markup.Common.Resources
    ( allStyles
    , allScripts
    ) where

import           BreadU.Pages.CSS.Own           ( ownCss )
import           BreadU.Pages.JS.Own            ( ownJS )
import           BreadU.Pages.JS.SocialButtons  ( twitterWidget )

import           Data.Text                      ( Text )
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes    as A

-- | All CSS we use, including our own style.
allStyles :: Html
allStyles = do
    -- Google Font
    styleFromCDN "https://fonts.googleapis.com/css?family=Comfortaa:400&amp;subset=cyrillic"
    -- Bootstrap tools.
    styleFromCDN "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.0.0-alpha.6/css/bootstrap.min.css"
    styleFromCDN "https://cdnjs.cloudflare.com/ajax/libs/mdbootstrap/4.3.2/css/mdb.min.css"
    -- Font Awesome.
    styleFromCDN "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css" 
    -- Own own style.
    style $ toHtml ownCss 
  where
    styleFromCDN :: Text -> Html
    styleFromCDN url = link ! A.rel "stylesheet" ! A.href (toValue url)

-- | All JavaScript we use, including our own script.
allScripts :: Html
allScripts = do
    -- JQuery.
    scriptFromCDN "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.2.1/jquery.min.js"
    -- Bootstrap tools.
    scriptFromCDN "https://cdnjs.cloudflare.com/ajax/libs/tether/1.4.0/js/tether.min.js"
    scriptFromCDN "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.0.0-alpha.6/js/bootstrap.min.js"
    scriptFromCDN "https://cdnjs.cloudflare.com/ajax/libs/mdbootstrap/4.3.2/js/mdb.min.js"
    -- Own own JS.
    script $ toHtml ownJS
    -- Widgets for social buttons.
    script $ toHtml twitterWidget
  where
    scriptFromCDN :: Text -> Html
    scriptFromCDN url  = script ! A.src (toValue url) $ mempty
