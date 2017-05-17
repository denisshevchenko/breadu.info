{-|
Module      : BreadU.Tools.FoodSuggestions
Description : Food suggestions for autocomplete.
Stability   : experimental
Portability : POSIX

Work with food suggestions for autocomplete when user typing food name.
-}

module BreadU.Tools.FoodSuggestions
    ( foodSuggestions
    ) where

import           BreadU.Types       ( FoodName
                                    , FoodNamePart
                                    , OrderedFoodNames
                                    )

import           Data.Vector        ( findIndex, (!), (!?) )
import           Data.Text          ( isPrefixOf )
import qualified Data.Text          as T
import           Data.Maybe         ( maybe )

-- | Suggestions for autocomplete of the food name. Can be empty.
foodSuggestions :: OrderedFoodNames -> FoodNamePart -> [FoodName]
foodSuggestions orderedNames foodNamePart = if T.null foodNamePart then [] else suggestions
  where
    suggestions = maybe [] findEnoughSuggestionsIfExist indexOfMatched
    indexOfMatched = findIndex foodBeginsWithPart orderedNames
    
    -- We already know that at least one suggestion is here. Try to find another ones.
    findEnoughSuggestionsIfExist :: Int -> [FoodName]
    findEnoughSuggestionsIfExist firstIndex =
        (orderedNames ! firstIndex) : findNextSuggestionsIfExist (firstIndex + 1) attempts
      where
        attempts = 2 -- First suggestion + 2 next ones, if exists.

    -- Since names are ordered, additional suggestions are following ones.
    findNextSuggestionsIfExist :: Int -> Int -> [FoodName]
    findNextSuggestionsIfExist _ 0 = [] -- No more attempts, stop.
    findNextSuggestionsIfExist currentIndex nthAttempt = case orderedNames !? currentIndex of
        Nothing -> []
        Just currentSuggestion ->
            if foodNamePart `isPrefixOf` currentSuggestion
                -- Insert current suggention and go forward.
                then currentSuggestion : findNextSuggestionsIfExist (currentIndex + 1) (nthAttempt - 1)
                else [] -- Don't match anymore, stop.

    -- If it begins with the current part of name - great, we work with it.
    foodBeginsWithPart :: FoodName -> Bool
    foodBeginsWithPart foodName = foodNamePart `isPrefixOf` foodName
