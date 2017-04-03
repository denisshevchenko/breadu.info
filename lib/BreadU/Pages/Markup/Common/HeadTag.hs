{-|
Module      : BreadU.Pages.Markup.Common.Head
Description : HTML markup for the <head>-section.
Stability   : experimental
Portability : POSIX

HTML markup for the <head>-section, for all pages.
-}

module BreadU.Pages.Markup.Common.HeadTag
    ( commonHeadTag
    ) where

import           BreadU.Pages.Markup.Common.Resources   ( allStyles )
import           BreadU.API                             ( staticImages )

import           Prelude                                hiding ( head )
import           Data.Text                              ( Text )
import           Data.Monoid                            ( (<>) )
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes            as A

-- | <head>-tag for all pages.
commonHeadTag :: Text -> Text -> Html
commonHeadTag aTitle metaDescription =
    head $ do
        title $ toHtml aTitle
        -- Correct encoding, just in case.
        meta ! A.charset "utf-8"
        -- For correct view on mobile displays.
        meta ! A.name "viewport"
             ! A.content "width=device-width, initial-scale=1.0"
        -- Service description, for Google search results.
        meta ! A.name "description"
             ! A.content (toValue metaDescription)
        -- For IE.
        meta ! A.httpEquiv "x-ua-compatible" 
             ! A.content "ie=edge"
        -- favicon.
        link ! A.rel "icon"
             ! A.type_ "image/png"
             ! A.href (toValue $ staticImages <> "favicon.png")
        -- Load all CSS we need.
        allStyles 
