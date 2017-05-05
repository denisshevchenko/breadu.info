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

-- | Content type for <header>-section, including About modal window.
data HeaderContent = HeaderContent
    { metaDescription     :: Text -- ^ <meta>-description for a page.
    , switchToRuTitle     :: Text -- ^ Title for switcher to Ru-language. 
    , switchToEnTitle     :: Text -- ^ Title for switcher to En-language.
    , aboutLabel          :: Text -- ^ Label for About button.
    , aboutTitle          :: Text -- ^ Title for About button.
    , siteName            :: Text -- ^ Site name.
    , siteTitle           :: Text -- ^ <title>-value for index page.
    , briefDescription    :: Text -- ^ Brief description in About modal window.
    , buDescription       :: Text -- ^ BU description in About modal window.
    , howToUseItTitle     :: Text -- ^ HowToUse description (1) in About modal window.
    , howToUseItFood      :: Text -- ^ HowToUse description (2) in About modal window.
    , howToUseItQuantity  :: Text -- ^ HowToUse description (3) in About modal window.
    , howToUseItAddFood   :: Text -- ^ HowToUse description (4) in About modal window.
    , howToUseItCalculate :: Text -- ^ HowToUse description (5) in About modal window.
    , disclaimerTitle     :: Text -- ^ Disclaimer title in About modal window.
    , disclaimer          :: Text -- ^ Disclaimer title in About modal window.
    , ossTitle            :: Text -- ^ OSS title in About modal window.
    , oss                 :: Text -- ^ OSS description in About modal window.
    , githubTitle         :: Text -- ^ Title for GitHub link.
    }

-- | Content type for <footer>-section.
data FooterContent = FooterContent
    { authorName          :: Text -- ^ Author name with copyright.
    , emailToAuthor       :: Text -- ^ Title for mailto-link.
    , tweetLabel          :: Text -- ^ Label for Tweet button.
    }

-- | Content type for HTTP 404.
data ContentHTTP404 = ContentHTTP404
    { error404PageTitle     :: Text -- ^ <title>-value for 404 page.
    , error404Description   :: Text -- ^ 404 description.
    , error404GoToIndexPage :: Text -- ^ 404 go to the index page.
    , error404IndexPageName :: Text -- ^ 404 link to the index page.
    }

-- | Content type for <body>-section of the index page.
data IndexBodyContent = IndexBodyContent
    { foodNameLabel       :: Text -- ^ "Food name" input's label.
    , carbsLabel          :: Text -- ^ "Carbs" input's label.
    , buLabel             :: Text -- ^ "BU" input's label.
    , gramsLabel          :: Text -- ^ "Grams" input's label.
    , addFoodLabel        :: Text -- ^ "Add" button's label.
    , addFoodTitle        :: Text -- ^ "Add" button's title.
    , calculateLabel      :: Text -- ^ "Calculate" button's label.
    , calculateTitle      :: Text -- ^ "Calculate" button's title.
    , removeFoodItemTitle :: Text -- ^ Remove cross' title.
    , orAnotherValue      :: Text -- ^ "or" text, between inputs in pair.
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
