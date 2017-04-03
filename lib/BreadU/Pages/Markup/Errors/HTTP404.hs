{-|
Module      : BreadU.Pages.Markup.Errors.HTTP404
Description : HTML markup for the '404 Not Found' page.
Stability   : experimental
Portability : POSIX

HTML markup for '404 Not Found' page.
-}

module BreadU.Pages.Markup.Errors.HTTP404
    ( http404Markup
    ) where

import           BreadU.Pages.Types                     ( ContentHTTP404(..) )
import           BreadU.Pages.Markup.Common.HeadTag     ( commonHeadTag )
import           BreadU.Pages.Markup.Common.Utils       ( fa )
import           BreadU.Pages.CSS.Names                 ( ClassName(..) )

import           Prelude                                hiding ( div, span )
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes            as A

-- | HTML markup for '404 Not Found' page.
http404Markup :: ContentHTTP404 -> Html
http404Markup ContentHTTP404{..} = do
    docType
    html $ do
        commonHeadTag error404PageTitle noMetaDescription
        body $ div ! A.class_ (toValue Block404) $ do
            mark404
            div ! A.class_ (toValue Block404Description) $ do
                div $ toHtml error404Description
                div linkToIndexPage
  where
    noMetaDescription = ""

    mark404 = div ! A.class_ (toValue Block404Mark) $ do
        span "4"
        span ! A.class_ (toValue Block404Mark0) $ fa "fa-circle-o"
        span "4"

    -- User will be redirected to the fake index page and will be 
    -- immediately redirected to localized-version of the page.
    linkToIndexPage = do
        toHtml error404GoToIndexPage
        span " "
        a ! A.href "/" $ toHtml error404IndexPageName
        span "."
