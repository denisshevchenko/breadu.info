{-|
Module      : BreadU.Pages.Markup.Common.Footer
Description : HTML markup for pages' top area.
Stability   : experimental
Portability : POSIX

HTML markup for pages' top area.
Please don't confuse it with <head>-tag, it's defined in another module.
-}

module BreadU.Pages.Markup.Common.Footer
    ( commonFooter
    ) where

import           BreadU.Types                       ( LangCode(..) )
import           BreadU.Pages.Types                 ( FooterContent(..) )
import           BreadU.Pages.CSS.Names             ( ClassName(..) )
import           BreadU.Pages.Markup.Common.Utils
import           BreadU.Pages.JS.SocialButtons      ( facebookSDK )

import           Prelude                            hiding ( div, span )
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes        as A
import           TextShow                           ( showt )
import           Data.Monoid                        ( (<>) )

-- | Footer for all pages.
commonFooter :: FooterContent -> LangCode -> Html
commonFooter FooterContent{..} langCode = footer $ do
    authorInfo
    socialButtons
  where
    authorInfo = div ! A.class_ (toValue AuthorInfo) $ do
        span $ toHtml authorName
        span ! A.class_ (toValue AuthorInfoMailToSeparator) $ mempty
        a ! A.href "mailto:me@dshevchenko.biz"
          ! A.class_ (toValue MailToIcon)
          ! A.title (toValue emailToAuthor) $ fa "fa-envelope"

    socialButtons = div ! A.class_ (toValue SocialButtons) $
        row_ $ do
            div ! A.class_ "col-6 text-right" $ facebook
            div ! A.class_ "col-6" $ twitter
 
    -- | Obtained from Facebook SDK documentation.
    facebook = do
        div ! A.id "fb-root" $ mempty
        script $ toHtml $ facebookSDK langCode

        div ! A.class_ "fb-share-button"
            ! dataAttribute "href" (toValue $ "https://breadu.info/" <> showt langCode)
            ! dataAttribute "layout" "button"
            ! dataAttribute "size" "large"
            ! dataAttribute "mobile-iframe" "true" $
            a ! A.class_ "fb-xfbml-parse-ignore"
              ! customAttribute "target" "_blank"
              ! A.href (toValue $ "https://www.facebook.com/sharer/sharer.php?u=https%3A%2F%2Fbreadu.info%2F"
                                  <> showt langCode
                                  <> "&amp;src=sdkpreparse") $ mempty 

    -- | <a>-code obtained from the Twitter Developer Documentation.
    twitter = 
        a ! A.class_ "twitter-share-button"
          ! A.href "https://twitter.com/intent/tweet?hashtags=BreadUCalculator"
          ! dataAttribute "size" "large" $ mempty
