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

import           BreadU.Types                           ( ClientLanguage(..) ) 
import           BreadU.Pages.Types                     ( LangCode(..) )
import           BreadU.Pages.Markup.Errors.HTTP404     ( http404Markup )
import           BreadU.Pages.Content.Errors.HTTP404    ( http404Content )

import           Text.Blaze.Renderer.Utf8               ( renderMarkup )
import           Network.Wai                            ( Application, responseLBS )
import           Network.HTTP.Types                     ( status404 )
import           Network.HTTP.Types.Header
import qualified Data.Text                              as T
import           TextShow                               ( showt )

{-|
   HTTP '404 Not Found' error. Implemented at Servant level, but we cannot define it
   as other pages in API, because 404 page doesn't correspond any endpoint. ;-)
   Language of 404 page is based on the value of 'Accept-Language' HTTP header.
-}
http404 :: Maybe ClientLanguage -> Application
http404 langHeader _ sendResponse =
    sendResponse $ responseLBS status404 httpHeaders $ case langHeader of
    Just (ClientLanguage language) ->
        if showt Ru `T.isPrefixOf` language then page404 Ru else page404 En
    Nothing -> page404 En
  where
    page404 langCode = renderMarkup $ http404Markup (http404Content langCode)

    httpHeaders :: ResponseHeaders
    httpHeaders = [("Content-Type", "text/html; charset=UTF-8")]
