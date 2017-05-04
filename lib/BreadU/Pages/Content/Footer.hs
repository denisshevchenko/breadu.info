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

import           BreadU.Pages.Types     ( LangCode(..), FooterContent(..) )

-- | Localized content for page's footer .
footerContent :: LangCode -> FooterContent
footerContent Ru = FooterContent
    { authorName        = "© Денис Шевченко, 2017"
    , emailToAuthor     = "Напишите мне"
    , tweetLabel        = "Твитнуть"
    }
footerContent En = FooterContent
    { authorName        = "© 2017 Denis Shevchenko"
    , emailToAuthor     = "Email me"
    , tweetLabel        = "Tweet"
    }
