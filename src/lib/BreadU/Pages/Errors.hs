{-|
Module      : BreadU.Pages.Errors
Description : Pages for HTTP errors.
Stability   : experimental
Portability : Portable

Pages for HTTP errors. These pages formed by another way than other pages.
-}

module BreadU.Pages.Errors
    ( http404
    ) where

import           BreadU.Types                           ( ClientLanguage(..)
                                                        , LangCode(..)
                                                        , allLanguages
                                                        )
import           BreadU.Pages.Markup.Errors.HTTP404     ( http404Markup )
import           BreadU.Pages.Content.Errors.HTTP404    ( http404Content )

import           Text.Blaze.Renderer.Utf8               ( renderMarkup )
import           Network.Wai                            ( Application, responseLBS )
import           Network.HTTP.Types                     ( status404 )
import           Network.HTTP.Types.Header
import qualified Data.Text                              as T
import           Data.List                              ( find )
import           Data.Maybe                             ( fromMaybe )
import           TextShow                               ( showt )

{-|
   HTTP '404 Not Found' error. Implemented at Servant level, but we cannot define it
   as other pages in API, because 404 page doesn't correspond any endpoint. ;-)
   Language of 404 page is based on the value of 'Accept-Language' HTTP header.
-}
http404 :: Maybe ClientLanguage -> Application
http404 langHeader _ sendResponse =
    sendResponse $ responseLBS status404 httpHeaders $ page404 $
        case langHeader of
            Nothing -> En -- No Lang header, it's strange...
            Just (ClientLanguage language) ->
                fromMaybe En $ find (\lang -> showt lang `T.isPrefixOf` language) allLanguages
  where
    page404 = renderMarkup . http404Markup . http404Content

    httpHeaders :: ResponseHeaders
    httpHeaders = [("Content-Type", "text/html; charset=UTF-8")]
