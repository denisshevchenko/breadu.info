{-|
Module      : CalculatorSpec
Description : Calculator testing
Stability   : experimental
Portability : POSIX

Run all checks for the calculator.
-}

module CalculatorSpec
    ( calculatorTesting
    ) where

import           BreadU.Types               ( FoodInfo(..) )
import           BreadU.Pages.Names         ( ElementName(..) )
import           BreadU.Tools.Calculator    ( calculate )

import           Data.HashMap.Strict        ( empty )
import           TextShow                   ( showt )
import           Test.Hspec

calculatorTesting :: Spec
calculatorTesting = do
    describe "Calculator testing" $ do
        it "Should be actually empty result" $ do
            calculate (FoodInfo []) empty `shouldBe` withoutActualResult
  where
    withoutActualResult = [(showt TotalBUQuantityId, "0.00")]
