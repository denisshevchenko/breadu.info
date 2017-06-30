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
                                                , CompleteFoods
                                                , FoodSuggestions(..)
                                                , FoodInfoFromForm(..)
                                                , InputedFoodInfo(..)
                                                , ClientLanguage(..)
                                                , NewFood(..)
                                                , CalculationResult(..)
                                                , LangCode(..)
                                                , allLanguages
                                                )
import           BreadU.Pages.Types             ( IndexPage(..) )
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
import           Data.List                      ( find, lookup )
import           Data.Text                      ( isPrefixOf, isInfixOf )
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( (<>) )

{-|
   This is fake index page. Actually it just redirects 
   immediately to a language-specific index page, based on
   the value of 'Accept-Language' HTTP-header from request.
-}
indexPageCommon :: Maybe ClientLanguage -> Handler IndexPage
indexPageCommon langHeader = return . RedirectTo $
    case langHeader of
        Nothing -> En -- No Lang header, it's strange...
        Just (ClientLanguage header) ->
            fromMaybe En $ find (\lang -> showt lang `isPrefixOf` header) allLanguages

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
autoComplete :: CompleteFoods -> InputedFoodInfo -> Handler FoodSuggestions
autoComplete commonFoods InputedFoodInfo{..} =
    case lookup currentLang commonFoods of
        Nothing -> error "Impossible: common food doesn't contain any supported language, fix your code."
        Just (orderedNames, _) -> return
            FoodSuggestions { suggestionsHTML = optionsForDatalist $ foodSuggestions orderedNames foodNamePart
                            }
  where
    currentLang = fromMaybe En $ find (\lang -> showt lang `isInfixOf` currentURL) allLanguages
