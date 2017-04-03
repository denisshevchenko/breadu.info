{-|
Module      : BreadU.Tools.CommonFood
Description : Work with common food.
Stability   : experimental
Portability : POSIX

Work with common food (default set of food every user will see).

Common food is obtaining from the 'food/common.csv' file.
It's simple .csv-file with header, format is:

@
     Food,CarbPer100g
     Белый хлеб,49.1
     White bread,49.1
@
-}

module BreadU.Tools.CommonFood
    ( loadCommonFood
    ) where

import           BreadU.Types                   ( FoodName, CarbPer100g, CompleteFood )
import           BreadU.Tools.Validators        ( valueOfCarbsIsValid, minCarbs, maxCarbs )

import           Data.List                      ( nub, sort )
import           Data.Csv                       ( HasHeader (..), decode )
import           Data.Monoid                    ( (<>) )
import qualified Data.ByteString.Lazy           as Lazy
import           Data.Vector                    ( Vector )
import qualified Data.Vector                    as V
import qualified Data.HashMap.Strict            as HM
import           Control.Exception              ( SomeException, catch )
import           Control.Monad                  ( when, unless, mapM_ )
import           System.Exit                    ( die )

-- | Loads common food from .csv-file and checks it valid.
loadCommonFood :: FilePath -> IO CompleteFood
loadCommonFood pathToCSV = do
    commonFood <- Lazy.readFile pathToCSV `catch` possibleProblems
    case extractFood commonFood of
        Left problem -> reportAboutInvalidCSV problem
        Right v -> do
            let pairs = V.toList v
            makeSureFoodIsValid pairs
            let foodMap = HM.fromList pairs
            return (V.fromList . sort . HM.keys $ foodMap, foodMap)
  where
    -- Cannot read the file, for example, because of wrong permissions.
    possibleProblems :: SomeException -> IO a
    possibleProblems whatHappened =
        die $ "I cannot open .csv-file with common food: " <> show whatHappened <> "."
    
    -- We have to specify explicitly what type of values we expect to see in .csv-file.
    -- In our case it's a pair FoodName + CarbPer100g.
    extractFood food = decode HasHeader food :: Either String (Vector (FoodName, CarbPer100g))

    reportAboutInvalidCSV problem =
        die $ "Something wrong with '" <> pathToCSV <> "' file: " <> show problem <> "."

-- | Food entities must be unique and carb values must be between 0.0 and 100.0.
makeSureFoodIsValid :: [(FoodName, CarbPer100g)] -> IO ()
makeSureFoodIsValid food =
    -- Monadic >> operator, there's no passing values from one function to other.
    makeSureItIsNonEmpty >> makeSureNoDuplication >> checkCarbValues
  where
    makeSureItIsNonEmpty :: IO ()
    makeSureItIsNonEmpty = when (null food) reportAboutEmptyCSV
    
    makeSureNoDuplication :: IO ()
    makeSureNoDuplication = do
        let foodNames = [foodName | (foodName, _) <- food]
            uniqueNames = nub foodNames
        when (length foodNames /= length uniqueNames) reportAboutDuplication
    
    -- We have to check carb values immediately.
    checkCarbValues :: IO ()
    checkCarbValues = mapM_ (\(_, carbPer100g) -> unless (valueOfCarbsIsValid carbPer100g) reportAboutInvalidCarbValue)
                            food

    reportAboutEmptyCSV =
        die "Empty .csv-file, please add at least one food."

    reportAboutDuplication =
        die "Food items in .csv-file must be unique, please remove duplication."

    reportAboutInvalidCarbValue =
        die $ "Carb per 100g cannot be less than " <> show minCarbs <> " and more than " <> show maxCarbs <> "."
