{-|
Module      : BreadU.Handlers
Description : Handlers for endpoints.
Stability   : experimental
Portability : POSIX

Handlers for all endpoints defined in API.
-}

module BreadU.Handlers
    ( indexPageCommon
    , indexPage
    , addFood
    , calculateFood
    , autoComplete
    ) where

import           BreadU.Types                   ( CompleteFood
                                                , FoodNamePart
                                                , FoodSuggestions(..)
                                                , FoodInfoFromForm(..)
                                                , ClientLanguage(..)
                                                , NewFood(..)
                                                , CalculationResult(..)
                                                )
import           BreadU.Pages.Types             ( IndexPage(..), LangCode(..) )
import           BreadU.Tools.FoodInfoParser    ( prepareFoodInfo )
import           BreadU.Tools.FoodSuggestions   ( foodSuggestions )
import           BreadU.Tools.Validators        ( validate )
import           BreadU.Tools.Calculator        ( calculate )
import           BreadU.Pages.Content.Header    ( headerContent )
import           BreadU.Pages.Content.Footer    ( footerContent )
import           BreadU.Pages.Content.IndexBody ( indexBodyContent )
import           BreadU.Pages.Markup.IndexPage  ( newFoodItem, optionsForDatalist )

import           Servant                        ( Handler )
import           System.Random                  ( getStdRandom, randomR )
import           Control.Monad.IO.Class         ( liftIO ) 
import           TextShow                       ( showt )
import           Data.Text                      ( isPrefixOf )
import           Data.Monoid                    ( (<>) )

{-|
   This is fake index page. Actually it just redirects 
   immediately to a language-specific index page, based on
   the value of 'Accept-Language' HTTP-header from request.
-}
indexPageCommon :: Maybe ClientLanguage -> Handler IndexPage
indexPageCommon langHeader = return . RedirectTo $
    case langHeader of
        Just (ClientLanguage header) ->
            if | showt Ru `isPrefixOf` header -> Ru
               | showt De `isPrefixOf` header -> De
               | otherwise                    -> En
        Nothing -> En

-- | Language-specific index page.
indexPage :: LangCode -> Handler IndexPage
indexPage lang = return
    IndexPage
        { langCode              = lang
        , headerContentForIndex = headerContent lang
        , bodyContentForIndex   = indexBodyContent lang
        , footerContentForIndex = footerContent lang
        }

-- | Handler for AJAX-request for adding a new food item.
addFood :: LangCode -> Handler NewFood
addFood lang = do
    anId <- liftIO newRandomId
    return NewFood { itemHTML = newFoodItem lang anId }
  where
      -- Random string is for unique id-attribute for a new food item.
    newRandomId = do
        randomNumber <- getStdRandom $ randomR (minNumber, maxNumber)
        -- Starts with a letter because of HTML4 restrictions, just in case.
        return $ "i" <> showt randomNumber

    -- 4-digits number is definitely enough. ;-)
    minNumber = 1000 :: Int
    maxNumber = 9999 :: Int

-- | Language-specific index page with response info (calculated result or validation report).
calculateFood :: LangCode
              -> CompleteFood
              -> FoodInfoFromForm
              -> Handler CalculationResult
calculateFood lang (_, food) (FoodInfoFromForm rawFoodInfo)
    | null badInputs' = return $ CalculationResult [] results'
    | otherwise       = return $ CalculationResult badInputs' []
  where
    badInputs' = validate foodInfo food lang
    foodInfo   = prepareFoodInfo rawFoodInfo
    results'   = calculate foodInfo food

-- | Handler for AJAX-request during typing food name. We return a few food suggestions.
autoComplete :: CompleteFood -> FoodNamePart -> Handler FoodSuggestions
autoComplete (orderedNames, _) foodNamePart = return
    FoodSuggestions
        { suggestionsHTML = optionsForDatalist $ foodSuggestions orderedNames foodNamePart
        }
