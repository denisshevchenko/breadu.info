{-|
Module      : BreadU.Pages.Content.Errors.HTTP404
Description : Language-specific content for "404 Not Found" page.
Stability   : experimental
Portability : POSIX

Language-specific content for "404 Not Found" page.
Fields of type 'ContentHTTP404' will be used in this page's markup.
-}

module BreadU.Pages.Content.Errors.HTTP404
    ( http404Content
    ) where

import           BreadU.Pages.Types     ( LangCode(..), ContentHTTP404(..) )

-- | Localized content for 404 page.
http404Content :: LangCode -> ContentHTTP404
http404Content Ru = ContentHTTP404
    { error404PageTitle     = "Ошибка 404"
    , error404Description   = "Похоже, страницы, которую вы ищите, здесь нет."
    , error404GoToIndexPage = "Пожалуйста, вернитесь на"
    , error404IndexPageName = "главную страницу"
    }
http404Content En = ContentHTTP404
    { error404PageTitle     = "404 Error"
    , error404Description   = "The page you were looking for doesn't exist."
    , error404GoToIndexPage = "Please return to the"
    , error404IndexPageName = "home page"
    }
http404Content De = ContentHTTP404
    { error404PageTitle     = "404 Fehler"
    , error404Description   = "Die Seite, die Sie gesucht haben, existiert nicht."
    , error404GoToIndexPage = "Bitte kehren Sie zur"
    , error404IndexPageName = "Startseite zurück"
    }
