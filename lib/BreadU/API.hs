{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : BreadU.API
Description : API for a server.
Stability   : experimental
Portability : POSIX

Servant-based API for a server. API consists of endpoints, each endpoint
will be handled by corresponding handler, see "BreadU.Handlers" module.
-}

module BreadU.API
    ( API
    , api
    , indexPageLink
    , addFoodLink
    , calculateFoodLink
    , autoCompleteLink
    , staticLink
    , staticImages
    ) where

import           BreadU.Pages.Types     ( LangCode(..), IndexPage(..) )
import           BreadU.Types           ( ClientLanguage(..)
                                        , FoodInfoFromForm(..)
                                        , NewFood(..)
                                        , CalculationResult(..)
                                        , FoodNamePart
                                        , FoodSuggestions(..)
                                        )

import           Servant
import           Servant.HTML.Blaze     ( HTML )
import           Servant.Utils.Links    ( safeLink )
import           TextShow               ( TextShow(..), fromString )
import           Data.Text              ( Text )
import           Data.Proxy             ( Proxy )
import           Data.Monoid            ( (<>) )

-- | Checks client language, based on value of 'Accept-Language' HTTP header.
type CheckClientLanguage = Header "Accept-Language" ClientLanguage

-- | Endpoint for fake root-variant of index page. User will be redirected
-- immediately to language-specific variant of the index page.
-- GET-request, HTML response.
type IndexPageRootEndpoint =
    CheckClientLanguage :> Get '[HTML] IndexPage

-- | Endpoint for an index page.
-- GET-request, HTML-response.
type IndexPageEndpoint =
    Get '[HTML] IndexPage

-- | Endpoint for AJAX-request, for adding a new food item.
-- You can think of :> operator as of '/' in the URL.
-- POST-request, JSON-response.
type AddFoodEndpoint =
    "add" :> Post '[JSON] NewFood 

-- | Endpoint for AJAX-request, for food calculating.
-- POST-request with form-url-encoded values from food form, JSON-response.
type CalculateFoodEndpoint =
    "calculate" :> ReqBody '[FormUrlEncoded] FoodInfoFromForm
                :> Post '[JSON] CalculationResult

-- | Endpoint for AJAX-request, for autocomplete during food name typing.
-- POST-request with plain text (food part), JSON-response.
type AutoCompleteEndpoint =
    "autocomplete" :> ReqBody '[PlainText] FoodNamePart
                   :> Post '[JSON] FoodSuggestions

-- | Endpoint to serve static content (see 'static' directory in the root of repository).
type StaticEndpoint =
    "static" :> Raw

-- | Endpoint for the custom '404 Not Found' page.
type NotFoundEndpoint =
    CheckClientLanguage :> Raw

-- | Types for prefixes of language-specific endpoints. Don't confuse it
-- with 'LangCode' type! These types are for Servant "type-level magic" only,
-- actually it's a type-level literals.
type EN = "en"
type DE = "de"
type RU = "ru"

-- | Type for whole API, we combine types for each endpoint into a single type 'API'.
-- You can think of :<|> operator as of 'OR' operator.
type API =
         IndexPageRootEndpoint
    :<|> RU :> IndexPageEndpoint
    :<|> RU :> AddFoodEndpoint
    :<|> RU :> CalculateFoodEndpoint
    :<|> EN :> IndexPageEndpoint
    :<|> EN :> AddFoodEndpoint
    :<|> EN :> CalculateFoodEndpoint
    :<|> DE :> IndexPageEndpoint
    :<|> DE :> AddFoodEndpoint
    :<|> DE :> CalculateFoodEndpoint
    :<|> AutoCompleteEndpoint
    :<|> StaticEndpoint
    :<|> NotFoundEndpoint

-- | Proxy for API. We use it for actual serving and for creating safe links.
api :: Proxy API
api = Proxy

{-|
   Safe links to endpoints. We use these links in pages' markup
   to define hrefs that are guaranteed valid. For convenience
   we represent them as a Text-based links.
-}
indexPageLink :: LangCode -> Text
indexPageLink En = showt $ safeLink api (Proxy @(EN :> IndexPageEndpoint))
indexPageLink De = showt $ safeLink api (Proxy @(DE :> IndexPageEndpoint))
indexPageLink Ru = showt $ safeLink api (Proxy @(RU :> IndexPageEndpoint))

addFoodLink :: LangCode -> Text
addFoodLink En = showt $ safeLink api (Proxy @(EN :> AddFoodEndpoint))
addFoodLink De = showt $ safeLink api (Proxy @(DE :> AddFoodEndpoint))
addFoodLink Ru = showt $ safeLink api (Proxy @(RU :> AddFoodEndpoint))

calculateFoodLink :: LangCode -> Text
calculateFoodLink En = showt $ safeLink api (Proxy @(EN :> CalculateFoodEndpoint))
calculateFoodLink De = showt $ safeLink api (Proxy @(DE :> CalculateFoodEndpoint))
calculateFoodLink Ru = showt $ safeLink api (Proxy @(RU :> CalculateFoodEndpoint))

autoCompleteLink :: Text
autoCompleteLink = showt $ safeLink api (Proxy @AutoCompleteEndpoint)

staticLink :: Text
staticLink = showt $ safeLink api (Proxy @StaticEndpoint)

-- | Instance for convert endpoints' paths to Text (without double quotes).
instance TextShow URI where
    showb = fromString . uriPath

-- | Helper, path to static images.
staticImages :: Text
staticImages = staticLink <> "/images/"
