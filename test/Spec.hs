{-|
Module      : Spec
Description : Root testing module
Stability   : experimental
Portability : POSIX

Run all specs.
-}

import           CalculatorSpec     ( calculatorTesting )

import           Test.Hspec         ( hspec )

main :: IO ()
main = hspec $ do
    calculatorTesting
