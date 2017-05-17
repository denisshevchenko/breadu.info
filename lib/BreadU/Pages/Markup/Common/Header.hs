{-|
Module      : BreadU.Pages.Markup.Common.Header
Description : HTML markup for pages' top area.
Stability   : experimental
Portability : POSIX

HTML markup for pages' top area.
Please don't confuse it with <head>-tag, it's defined in another module.
-}

module BreadU.Pages.Markup.Common.Header
    ( commonHeader
    ) where

import           BreadU.Types                       ( LangCode(..), allLanguages )
import           BreadU.Pages.Types                 ( HeaderContent(..) )
import           BreadU.Pages.CSS.Names             ( ClassName(..) )
import           BreadU.API                         ( indexPageLink )
import           BreadU.Pages.Markup.Common.Utils

import           Prelude                            hiding ( div, span )
import           TextShow                           ( showt )
import           Data.Text                          ( toUpper )
import           Data.List                          ( delete )
import           Data.Monoid                        ( (<>) )
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes        as A

-- | Header for all pages.
commonHeader :: HeaderContent -> LangCode -> Html
commonHeader headerContent@HeaderContent{..} langCode = header $ do
    row_ $ do
        aboutModal headerContent
        col_6 languageSwitcher
        col_6 about
    h1 $ toHtml siteName
  where
    -- All supported languages are here.
    languageSwitcher =
        div ! A.class_ "btn-group" $ do
            button ! A.class_ (toValue $ "btn btn-outline-info btn-sm btn-rounded waves-effect dropdown-toggle "
                                         <> showt LanguageSwitcher)
                   ! A.type_ "button"
                   ! dataAttribute "toggle" "dropdown"
                   ! customAttribute "aria-haspopup" "true"
                   ! customAttribute "aria-expanded" "false" $ toHtml $ showt langCode

            div ! A.class_ "dropdown-menu" $
                mapM_ (\lCode -> a ! A.class_ "dropdown-item"
                                   ! A.href (toValue $ indexPageLink lCode) $
                                     toHtml . toUpper . showt $ lCode)
                      otherLanguages
    
    otherLanguages = delete langCode allLanguages

    about = div ! A.class_ "text-right" $
        button ! A.type_ "button"
               ! A.class_ "btn btn-outline-info btn-sm btn-rounded waves-effect"
               ! A.title (toValue aboutTitle)
               ! dataAttribute "toggle" "modal"
               ! dataAttribute "target" "#aboutModal" $ toHtml aboutLabel

-- | Show modal window with info about the service.
aboutModal :: HeaderContent -> Html
aboutModal HeaderContent{..} =
    div ! A.class_ "modal"
        ! A.id "aboutModal"
        ! A.tabindex "-1"
        ! customAttribute "role" "dialog"
        ! customAttribute "aria-labelledby" "aboutModalLabel"
        ! customAttribute "aria-hidden" "true" $
        div ! A.class_ "modal-dialog modal-lg"
            ! customAttribute "role" "document" $
            div ! A.class_ "modal-content" $ do
                div ! A.class_ "modal-header" $ do
                    h3 ! A.class_ "modal-title w-100"
                       ! A.id "aboutModalLabel" $ toHtml siteName
                    closeButton

                div ! A.class_ (toValue $ "modal-body " <> showt AboutInfo) $ do
                    p $ toHtml briefDescription
                    p $ toHtml buDescription

                    h4 $ toHtml howToUseItTitle
                    p $ toHtml howToUseItFood
                    p $ toHtml howToUseItQuantity
                    p $ toHtml howToUseItAddFood
                    p $ toHtml howToUseItCalculate

                    h4 $ toHtml ossTitle
                    p $ preEscapedToHtml oss
                    
                    h4 $ toHtml disclaimerTitle
                    p $ toHtml disclaimer
  where
    closeButton = 
        button ! A.type_ "button"
               ! A.class_ "close"
               ! dataAttribute "dismiss" "modal"
               ! customAttribute "aria-label" "Close" $
            span ! customAttribute "aria-hidden" "true" $
                preEscapedToHtml ("&times;" :: String)
