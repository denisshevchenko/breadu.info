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

import           BreadU.Pages.Types                 ( FooterContent(..) )
import           BreadU.Pages.CSS.Names             ( ClassName(..) )
import           BreadU.Pages.Markup.Common.Utils

import           Prelude                            hiding ( div, span )
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes        as A

-- | Footer for all pages.
commonFooter :: FooterContent -> Html
commonFooter FooterContent{..} = footer $
    row_ $ do
        div ! A.class_ "col-xl-8 col-lg-8 col-md-8 col-sm-7 col-xs-12" $ authorInfo
        div ! A.class_ "col-xl-4 col-lg-4 col-md-4 col-sm-5 col-xs-12" $ socialButtons
  where
    authorInfo = div ! A.class_ (toValue AuthorInfo) $ do
        span $ toHtml authorName
        
        span ! A.class_ (toValue AuthorInfoMailToSeparator) $ mempty
        
        a ! A.href "mailto:me@dshevchenko.biz"
          ! A.class_ (toValue MailToIcon)
          ! A.title (toValue emailToAuthor) $ fa "fa-envelope"

    socialButtons = div ! A.class_ (toValue SocialButtons) $
        twitter
    
    -- | <a>-code obtained from the Twitter Developer Documentation.
    twitter = 
        a ! A.class_ "twitter-share-button"
          ! A.href "https://twitter.com/intent/tweet?hashtags=BreadUCalculator"
          ! dataAttribute "size" "large" $ toHtml tweetLabel
