{-|
Module      : BreadU.Tools.FoodInfoParser
Description : Food form info parser.
Stability   : experimental
Portability : POSIX

Parser of the info from the food form.
-}

module BreadU.Tools.FoodInfoParser
    ( prepareFoodInfo
    ) where

import           BreadU.Types           ( RawFoodInfo
                                        , FoodInfo(..)
                                        , FoodItem(..)
                                        , Input
                                        )
import           BreadU.Pages.Names     ( ElementName(..) )

import           TextShow               ( showt )
import           Data.List.Split        ( chunksOf )
import           Data.List              ( sort, partition, find )
import qualified Data.Text              as T

-- | Prepares raw list of inputs from the main food form into convenient type.
prepareFoodInfo :: RawFoodInfo -> FoodInfo
prepareFoodInfo rawFoodInfo = FoodInfo { items = map prepareFoodItem allItems }
  where
    -- Convert into sorted list of 'Input'.
    -- Raw values from the form's inputs are stripped, because strictly speaking, spaces isn't a problem.
    inputs :: [Input]
    inputs = sort [ (name, let withoutSpaces = T.strip value in if T.null withoutSpaces then Nothing else Just withoutSpaces)
                  | (name, [value]) <- rawFoodInfo -- We definitely know there's single value for named input.
                  ]
    
    -- Extract first food item and other items, so we have two inputs list as (A, B),
    -- where A is inputs for first item and B is inputs for other items.
    ( firstItem
      , otherItems ) = partition (\(name, _) -> showt FirstFoodPrefix `T.isPrefixOf` name) inputs

    -- We split inputs for items items with chunks, 4 inputs in each,
    -- because we know that each food always represented by 4 inputs.
    -- As a result we have a list where each element is a list of inputs
    -- for one food. Result list can be empty.
    allItems :: [[Input]]
    allItems = firstItem : chunksOf 4 otherItems

    -- Prepare convenient food item.
    prepareFoodItem :: [Input] -> FoodItem
    prepareFoodItem inputs' = FoodItem
        { foodName    = extractValueBy FoodNameInputPostfix inputs'
        , carbPer100g = extractValueBy CarbsInputPostfix inputs'
        , bu          = extractValueBy BUInputPostfix inputs'
        , grams       = extractValueBy GramsInputPostfix inputs'
        }

    -- Extract value of input by particular postfix.
    extractValueBy :: ElementName -> [Input] -> Input
    extractValueBy namePostfix inputs' = input
      where
          -- We definitely know that such an input is here, because we form HTML ourselves. ;-)
          -- So extract from 'Just'-variant.
        Just input = find (\(name, _) -> showt namePostfix `T.isSuffixOf` name) inputs'
