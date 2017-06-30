{-|
Module      : BreadU.Tools.Calculator
Description : BU <-> Grams calculator
Stability   : experimental
Portability : POSIX

BU <-> Grams calculator. At this point we already have
food values and they're valid, so we just convert them.
-}

module BreadU.Tools.Calculator
    ( calculate
    ) where

import           BreadU.Types                   ( CarbPer100g
                                                , BU
                                                , Grams
                                                , Food
                                                , FoodInfo(..)
                                                , FoodItem(..)
                                                , Result
                                                )
import           BreadU.Pages.Names             ( ElementName(..) )

import qualified Data.HashMap.Strict            as HM
import           TextShow                       ( showt )
import           Data.Double.Conversion.Text    ( toFixed )
import           Data.Text.Read                 ( double )
import           Data.Text                      ( Text, isSuffixOf, replace )
import           Data.Maybe                     ( isJust, fromJust, catMaybes, fromMaybe )
import           Data.List                      ( sum )

-- | Calculate BU/Grams values. 
calculate :: FoodInfo -> Food -> [Result]
calculate FoodInfo{..} commonFood = totalBUValue : buAndGramsResults -- Total BU value is always first, for simplicity.
  where
    buAndGramsResults = concatMap calculateItemValues items

    -- Take all BU values, sum them as numbers and convert again
    -- to the 'Text' with the same rounding.
    totalBUValue :: Result
    totalBUValue = (showt TotalBUQuantityId, toFixed 2 buSum)
      where
        buSum = sum [asDouble buValue | (_, buValue) <- filter buOnly buAndGramsResults]
        buOnly (inputId, _) = showt BUInputPostfix `isSuffixOf` inputId

    calculateItemValues :: FoodItem -> [Result]
    calculateItemValues FoodItem{..} = catMaybes $ carbsValue : buAndGramsValues
      where
        (_,            maybeFood)   = foodName
        (carbInputId,  maybeCarbs)  = carbPer100g
        (buInputId,    maybeBU)     = bu
        (gramsInputId, maybeGrams)  = grams

        foodNameIsHere = isJust maybeFood
        carbohydrates  = if foodNameIsHere
                             -- We already know that this food name exists in the 'Food'.
                             then fromMaybe (error "Impossible: no carbs value for known food, fix your code!")
                                            $ HM.lookup (fromJust maybeFood) commonFood
                             -- Take carbohydrates values from carbPer100g input.
                             else asDouble $ fromMaybe (error "Impossible: carbs should be here, fix your validation code!")
                                                       maybeCarbs

        carbsValue = if foodNameIsHere then Just (carbInputId, toFixed 1 carbohydrates) else Nothing

        buAndGramsValues = doCalculate maybeBU maybeGrams carbohydrates

        -- Convert grams to BU and vice versa. BU value is always here
        -- because of calculationg of the total BU value.
        doCalculate :: Maybe Text -> Maybe Text -> CarbPer100g -> [Maybe Result]
        doCalculate Nothing    (Just grams') carbs = [Just (buInputId, toFixed 2 $ convertGramsToBU carbs (asDouble grams'))]
        doCalculate (Just bu') Nothing       carbs = [ Just (buInputId, bu')
                                                     , Just (gramsInputId, toFixed 1 $ convertBUToGrams carbs (asDouble bu'))
                                                     ]
        doCalculate (Just bu') (Just _)      _     = [Just (buInputId, bu')]
        doCalculate Nothing    Nothing       _     = [Nothing]

asDouble :: Text -> Double
asDouble (double . anyDecimalSeparator -> Right (number, _)) = number
asDouble (double . anyDecimalSeparator -> _) = error "Impossible: text value already validated as a number, fix your code!"

-- | We support both decimal separators, point and comma.
anyDecimalSeparator :: Text -> Text
anyDecimalSeparator = replace "," "."

-- | Converter from BU to grams, based on carbohydrates value.
convertBUToGrams :: CarbPer100g -> BU -> Grams
convertBUToGrams carbPer100g breadUnits = breadUnits * carbIn1BU * 100.0 / carbPer100g

-- | Converter from grams to BU, based on carbohydrates value.
convertGramsToBU :: CarbPer100g -> Grams -> BU
convertGramsToBU carbPer100g grams = carbPer100g * grams / 100.0 / carbIn1BU

-- | Most diabetics guides report that 1 BU = 10-12 grams of digestible carbohydrates.
-- We take an average value, 1 BU = 11 grams of carbohydrates.
carbIn1BU :: Grams
carbIn1BU = 11.0
