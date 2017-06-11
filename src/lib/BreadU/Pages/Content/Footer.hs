{-|
Module      : BreadU.Pages.Content.Footer
Description : Language-specific content for page's footer.
Stability   : experimental
Portability : POSIX

Language-specific content for page's footer.
Fields of type 'FooterContent' will be used in pages' markup.
-}

module BreadU.Pages.Content.Footer
    ( footerContent
    ) where

import           BreadU.Types           ( LangCode(..) )
import           BreadU.Pages.Types     ( FooterContent(..) )

-- | Localized content for page's footer .
footerContent :: LangCode -> FooterContent
footerContent Ru = FooterContent
    { authorName        = "© Денис Шевченко, 2017"
    , emailToAuthor     = "Напишите мне"
    , blueCircleTitle   = "Да, у диабета есть эмблема! Узнайте больше..."
    }
footerContent En = FooterContent
    { authorName        = "© 2017 Denis Shevchenko"
    , emailToAuthor     = "Email me"
    , blueCircleTitle   = "Yes, diabetes has a logo! Learn more..."
    }
footerContent De = FooterContent
    { authorName        = "© 2017 Denis Shevchenko"
    , emailToAuthor     = "Maile mir"
    , blueCircleTitle   = "Ja, Diabetes hat ein Logo! Mehr wissen..."
    }
