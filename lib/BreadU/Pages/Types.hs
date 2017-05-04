{-|
Module      : BreadU.Pages.Types
Description : Skeleton types for pages.
Stability   : experimental
Portability : POSIX

This module contains "skeleton" types for all pages and their parts.
The core idea of serving HTML via Servant is really simple:

    1. We define skeleton type. This type itself is markup-agnostic and
       language-agnostic, it just represents the core essence of a page.
    2. We define 'ToMarkup' instance for skeleton type. It forms HTML markup for a page.
       Please see "BreadU.Pages.Markup.IndexPage" module for example.
    3. We fill skeleton type with a language-specific content.
       Please see "BreadU.Pages.Content.IndexBody" module for example.

So, final page = skeleton type + 'ToMarkup' instance + language-specific content.
Actual HTML-rendering occurs at Servant-layer, via HTML type, please see "BreadU.API" module for example.

Note that we're using 'blaze-html' rendering in this project, but Servant support 'lucid' rendering as well,
via 'servant-lucid' package.
-}

module BreadU.Pages.Types
    ( LangCode(..)
    , HeaderContent(..)
    , FooterContent(..)
    , IndexBodyContent(..)
    , ContentHTTP404(..)
    , IndexPage(..)
    ) where

import           Data.Text      ( Text )
import           TextShow       ( TextShow(..) )

-- | Represents supported languages.
data LangCode = Ru | En
    deriving (Eq)

-- | This instance allows us to convert values
-- of 'LangCode' type to the strict 'Text', via 'showt' function.
instance TextShow LangCode where
    showb Ru = "ru"
    showb En = "en"

-- | Content type for header section, including About modal window.
data HeaderContent = HeaderContent
    { metaDescription     :: Text
    , switchToRuTitle     :: Text
    , switchToEnTitle     :: Text
    , aboutLabel          :: Text
    , aboutTitle          :: Text
    , siteName            :: Text
    , siteTitle           :: Text
    , briefDescription    :: Text
    , buDescription       :: Text
    , howToUseItTitle     :: Text
    , howToUseItFood      :: Text
    , howToUseItQuantity  :: Text
    , howToUseItAddFood   :: Text
    , howToUseItCalculate :: Text
    , disclaimerTitle     :: Text
    , disclaimer          :: Text
    , ossTitle            :: Text
    , oss                 :: Text
    , githubTitle         :: Text
    }

data FooterContent = FooterContent
    { authorName          :: Text
    , emailToAuthor       :: Text
    , tweetLabel          :: Text
    }

-- | Content type for HTTP 404.
data ContentHTTP404 = ContentHTTP404
    { error404PageTitle     :: Text
    , error404Description   :: Text
    , error404GoToIndexPage :: Text
    , error404IndexPageName :: Text
    }

-- |
data IndexBodyContent = IndexBodyContent
    { foodNameLabel       :: Text -- ^
    , carbsLabel          :: Text -- ^
    , buLabel             :: Text -- ^
    , gramsLabel          :: Text -- ^
    , addFoodLabel        :: Text -- ^
    , addFoodTitle        :: Text -- ^
    , calculateLabel      :: Text -- ^
    , calculateTitle      :: Text -- ^
    , removeFoodItemTitle :: Text -- ^
    , orAnotherValue      :: Text -- ^
    }

-- | Skeleton type that represents index page.
data IndexPage
    = RedirectTo LangCode
    | IndexPage { langCode              :: LangCode
                -- ^ Language code for markup, attribute of <html>-tag.
                , headerContentForIndex :: HeaderContent
                -- ^ Language-specific content for index page header.
                , bodyContentForIndex   :: IndexBodyContent
                -- ^ Language-specific content for index page body.
                , footerContentForIndex :: FooterContent
                -- ^ Language-specific content for index page footer.
                }
