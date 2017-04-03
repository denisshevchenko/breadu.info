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

import qualified Data.HashMap.Strict            as HM
import           Data.Double.Conversion.Text    ( toFixed )
import           Data.Text.Read                 ( double )
import           Data.Text                      ( Text )
import           Data.Maybe                     ( isJust, fromJust )

-- | Calculate food values. In this point we definitely know that values are valid because of success validation.
calculate :: FoodInfo -> Food -> [Result]
calculate FoodInfo{..} commonFood = concatMap calculateItemValues items
  where
    calculateItemValues :: FoodItem -> [Result]
    calculateItemValues FoodItem{..} = carbsValue ++ doCalculate maybeBU maybeGrams carbohydrates
      where
        (_,            maybeFood)   = foodName
        (carbInputId,  maybeCarbs)  = carbPer100g
        (buInputId,    maybeBU)     = bu
        (gramsInputId, maybeGrams)  = grams

        foodNameIsHere = isJust maybeFood
        carbohydrates  = if foodNameIsHere
                             -- We already know that this food name exists in the 'Food'.
                             then fromJust $ HM.lookup (fromJust maybeFood) commonFood
                             -- Take carbohydrates values from carbPer100g input.
                             else let Right (number, _) = double . fromJust $ maybeCarbs
                                  in number

        carbsValue = if foodNameIsHere then [(carbInputId, roundAsText carbohydrates)] else []
        
        -- | Convert grams to BU and vice versa.
        doCalculate :: Maybe Text -> Maybe Text -> CarbPer100g -> [Result]
        doCalculate Nothing    (Just grams') carbs = [(buInputId,    roundAsText $ convertGramsToBU carbs (asDouble grams'))]
        doCalculate (Just bu') Nothing       carbs = [(gramsInputId, roundAsText $ convertBUToGrams carbs (asDouble bu'))]
        doCalculate (Just _)   (Just _)      _     = []
        doCalculate Nothing    Nothing       _     = []

        -- We definitely know that rawNumber contains a valid number.
        asDouble :: Text -> Double
        asDouble rawNumber = let Right (number, _) = double rawNumber in number
        
        roundAsText :: Double -> Text
        roundAsText = toFixed 1

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
