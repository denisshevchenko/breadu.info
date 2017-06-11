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

import           BreadU.Types                       ( CompleteFoods, LangCode(..) )
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
import           Data.Maybe                         ( fromJust )
import           Data.List                          ( lookup )

-- | Defines the server for serving an API.
-- Please note that an order of handlers must be exactly the same as in API type!
server :: CompleteFoods -> Server API
server commonFoods =
         indexPageCommon
    :<|> indexPage Ru
    :<|> addFood Ru
    :<|> calculateFood Ru (foodFor Ru)
    :<|> indexPage En
    :<|> addFood En
    :<|> calculateFood En (foodFor En)
    :<|> indexPage De
    :<|> addFood De
    :<|> calculateFood De (foodFor De)
    :<|> autoComplete commonFoods
    :<|> serveDirectory "/var/www" -- Our static content will be served by a real web server, I use Nginx.
    :<|> http404
  where
    -- We definitely know that food is here.
    foodFor lang = fromJust $ lookup lang commonFoods
