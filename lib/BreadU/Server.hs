{-|
Module      : BreadU.Server
Description : Server
Stability   : experimental
Portability : POSIX

Actual server. You can think of it as of a glue:
API defines endpoints, handlers handle these endpoints, and server combines API with handlers.
-}

module BreadU.Server
    ( server
    ) where

import           BreadU.Types                       ( CompleteFood )
import           BreadU.Pages.Types                 ( LangCode(..) )
import           BreadU.API                         ( API )

-- | Pages 'ToMarkup' instances, we need it to render complete HTML pages.
import           BreadU.Pages.Markup.IndexPage      ()

import           BreadU.Handlers                    ( indexPageCommon
                                                    , indexPage
                                                    , addFood
                                                    , calculateFood
                                                    , autoComplete
                                                    )
-- | Custom pages for HTTP errors.
import           BreadU.Pages.Errors                ( http404 )

import           Servant                            ( Server
                                                    , (:<|>)(..)
                                                    , serveDirectory
                                                    )

-- | Defines the server for serving an API.
-- Please note that an order of handlers must be exactly the same as in API type!
server :: CompleteFood -> Server API
server commonFood =
         indexPageCommon
    :<|> indexPage Ru
    :<|> addFood Ru
    :<|> calculateFood Ru commonFood
    :<|> indexPage En
    :<|> addFood En
    :<|> calculateFood En commonFood
    :<|> autoComplete commonFood
    :<|> serveDirectory "/var/www" -- Our static content will be served by a real web server, I use Nginx.
    :<|> http404
