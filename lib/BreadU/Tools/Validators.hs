{-# LANGUAGE MultiWayIf #-}

{-|
Module      : BreadU.Tools.Validators
Description : Validators for food values.
Stability   : experimental
Portability : POSIX

Validators for all food values submitted by user.
-}

module BreadU.Tools.Validators
    ( valueOfCarbsIsValid
    , minCarbs
    , maxCarbs
    , validate
    ) where

import           BreadU.Types                           ( CarbPer100g
                                                        , BU, Grams
                                                        , BadInput
                                                        , Food
                                                        , FoodInfo(..)
                                                        , FoodItem(..)
                                                        )
import           BreadU.Pages.Types                     ( LangCode(..) )
import qualified BreadU.Pages.Content.Errors.Messages   as M

import           Control.Monad.Trans.State.Strict       ( State, execState, modify )
import           Data.HashMap.Strict                    ( member )
import           Data.Maybe                             ( isNothing
                                                        , fromMaybe
                                                        , mapMaybe
                                                        , catMaybes
                                                        )
import           Data.Text.Read                         ( double )
import           Data.Text                              ( Text )
import qualified Data.Text                              as T

{-|
   "Log" for validation results: each validator stores its output in the bad inputs list.
   As you can see, we use pure State monad transformer (based on 'Identity' monad).
-}
type ValidationLog = State (FoodInfo, LangCode, [BadInput]) ()

{-|
   Validate food info submitted by user. The idea is very simple:
   we check actual info from each input and if it's bad, we
   form BadInput: info about the problem with localized message.
   This message will be shown near corresponding input as a popover.
-}
validate :: FoodInfo -> Food -> LangCode -> [BadInput]
validate foodInfo commonFood langCode = badInputs
  where
    (_, _, badInputs) = doValidation initialState
    initialState      = (foodInfo, langCode, [])
    doValidation      = execState $ checkMissedValues
                                 >> checkIncorrectNumbers
                                 >> checkUnknownFood commonFood
    -- Run State-transformer via execState, so all checkers work within the same
    -- State-context they extract food info and langCode from. Checkers will store
    -- in this State information about the problems they found.

-- | Check missed values. User must enter food (via name or carbPer100g) and quantity (in grams or in BU).
checkMissedValues :: ValidationLog
checkMissedValues =
    -- Modify values in the State-context. It's very convenient:
    -- we obtain current badInputs and append to it info about problems we found.
    modify $ \(foodInfo@FoodInfo{..}, langCode, badInputs) ->
        (foodInfo, langCode, badInputs ++ concatMap (inputsWithMissedValues langCode) items)
  where
    inputsWithMissedValues :: LangCode -> FoodItem -> [BadInput]
    inputsWithMissedValues langCode FoodItem{..} = catMaybes [badFood, badQuantity]
      where
        (foodInputId, maybeFood)  = foodName
        (_,           maybeCarbs) = carbPer100g
        (buInputId,   maybeBU)    = bu
        (_,           maybeGrams) = grams

        foodIsMissed     = isNothing maybeFood && isNothing maybeCarbs
        quantityIsMissed = isNothing maybeBU   && isNothing maybeGrams
        badFood          = if foodIsMissed     then Just missedFoodMessage     else Nothing
        badQuantity      = if quantityIsMissed then Just missedQuantityMessage else Nothing

        -- Here's an example:
        -- * 'foodInputId' - an id of the input where's incorrect value.
        -- * 'missedFoodErrorTitle' - localized title for error message.
        -- * 'missedFoodErrorMessage' - localized error message.
        missedFoodMessage     = ( foodInputId
                                , M.missedFoodErrorTitle langCode
                                , M.missedFoodErrorMessage langCode )

        missedQuantityMessage = ( buInputId
                                , M.missedQuantityErrorTitle langCode
                                , M.missedQuantityErrorMessage langCode )

-- | Check incorrect numbers (negative ones or out-of-range).
checkIncorrectNumbers :: ValidationLog
checkIncorrectNumbers =
    modify $ \(foodInfo@FoodInfo{..}, langCode, badInputs) ->
        (foodInfo, langCode, badInputs ++ concatMap (inputsWithIncorrectNumbers langCode) items)
  where
    inputsWithIncorrectNumbers :: LangCode -> FoodItem -> [BadInput]
    inputsWithIncorrectNumbers langCode FoodItem{..} = catMaybes [badCarbs, badBU, badGrams]
      where  
        (carbInputId,  maybeCarbs) = carbPer100g
        (buInputId,    maybeBU)    = bu
        (gramsInputId, maybeGrams) = grams
        badCarbs = onlyBad valueOfCarbsIsValid carbIncorrectNumber  $ fromMaybe "" maybeCarbs
        badBU    = onlyBad valueOfBUIsValid    buIncorrectNumber    $ fromMaybe "" maybeBU
        badGrams = onlyBad valueOfGramsIsValid gramsIncorrectNumber $ fromMaybe "" maybeGrams

        onlyBad :: (Double -> Bool) -> BadInput -> Text -> Maybe BadInput
        onlyBad predicate badInput rawValue = case double rawValue of
            Right (number, remainingTrash) ->
                if | notEmpty remainingTrash -> Just badInput
                   | not $ predicate number  -> Just badInput
                   | otherwise               -> Nothing
            Left _ ->
                if | notEmpty rawValue -> Just badInput
                   | otherwise         -> Nothing

        notEmpty = not . T.null
        
        carbIncorrectNumber  = ( carbInputId
                               , M.carbIncorrectNumberErrorTitle langCode
                               , M.carbIncorrectNumberErrorMessage langCode minCarbs maxCarbs )
        
        buIncorrectNumber    = ( buInputId
                               , M.buIncorrectNumberErrorTitle langCode
                               , M.buIncorrectNumberErrorMessage langCode )
        
        gramsIncorrectNumber = ( gramsInputId
                               , M.gramsIncorrectNumberErrorTitle langCode
                               , M.gramsIncorrectNumberErrorMessage langCode )

-- | Check if user inserted unknown food name(s).
checkUnknownFood :: Food -> ValidationLog
checkUnknownFood commonFood =
    modify $ \(foodInfo@FoodInfo{..}, langCode, badInputs) ->
        (foodInfo, langCode, badInputs ++ mapMaybe (inputsWithUnknownFood langCode) items)
  where
    inputsWithUnknownFood :: LangCode -> FoodItem -> Maybe BadInput
    inputsWithUnknownFood langCode FoodItem{..} =
        case maybeFood of
            Just food -> if not $ member food commonFood then Just foodUnknownName else Nothing
            Nothing -> Nothing
      where
        (foodInputId, maybeFood) = foodName
        foodUnknownName = ( foodInputId
                          , M.foodUnknownNameErrorTitle langCode
                          , M.foodUnknownNameErrorMessage langCode )

-- | BU value must be positive integer or decimal number.
valueOfBUIsValid :: BU -> Bool
valueOfBUIsValid breadUnits = breadUnits > 0.0

-- | Grams value must be positive integer or decimal number.
valueOfGramsIsValid :: Grams -> Bool
valueOfGramsIsValid grams = grams > 0.0

-- | Carbs value must be integer or decimal number between min and max.
-- Assumed that carbs value cannot be precisely equal to 0.0 nor 100.0.
-- Even white sugar has 99.98 carbs value, but not 100.0.
valueOfCarbsIsValid :: CarbPer100g -> Bool
valueOfCarbsIsValid carbPer100g = carbPer100g > minCarbs && carbPer100g < maxCarbs

minCarbs, maxCarbs :: CarbPer100g
minCarbs = 0.0
maxCarbs = 100.0
