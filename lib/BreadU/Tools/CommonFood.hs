{-|
Module      : BreadU.Tools.CommonFood
Description : Work with common food.
Stability   : experimental
Portability : POSIX

Work with common food (default set of food every user will see).

Common food is obtaining from the 'food/common/*.csv' files.
It's simple .csv-files with headers, format is:

@
     Food,CarbPer100g
     Белый хлеб,49.1
     White bread,49.1
@
-}

module BreadU.Tools.CommonFood
    ( loadCommonFood
    ) where

import           BreadU.Types                   ( FoodName
                                                , CarbPer100g
                                                , CompleteFood
                                                , CompleteFoods
                                                , LangCode(..)
                                                , allLanguages
                                                )
import           BreadU.Tools.Validators        ( valueOfCarbsIsValid, minCarbs, maxCarbs )

import           Data.Char                      ( toUpper ) 
import qualified Data.Text                      as T 
import           Data.Maybe                     ( isNothing, fromJust )
import           Data.List                      ( nub, sort, find )
import           Data.Csv                       ( HasHeader (..), decode )
import           Data.Monoid                    ( (<>) )
import qualified Data.ByteString.Lazy           as Lazy
import           Data.Vector                    ( Vector )
import qualified Data.Vector                    as V
import qualified Data.HashMap.Strict            as HM
import           Control.Exception              ( SomeException, catch )
import           Control.Monad                  ( when, unless, mapM_, forM )
import           System.FilePath.Posix          ( dropExtension, (</>) )
import           System.Directory               ( listDirectory )
import           System.Exit                    ( die )

-- |
loadCommonFood :: FilePath -> IO CompleteFoods
loadCommonFood pathToCSVDir = listDirectory pathToCSVDir >>= \allFiles -> do
    csvs <- makeSureWeHaveCSVForAllLanguages allFiles
    forM csvs $ \(language, csv) -> do
        foodForThisLanguage <- loadCommonFoodFromOneFile (pathToCSVDir </> csv)
        return (language, foodForThisLanguage)

-- | We have to check if .csv-files for all supported languages are here.
makeSureWeHaveCSVForAllLanguages :: [FilePath] -> IO [(LangCode, FilePath)]
makeSureWeHaveCSVForAllLanguages allFiles = forM allLanguages $ \language -> do
    let csv = find (\file -> dropExtension file == show language) allFiles
    when (isNothing csv) $ reportAboutMissingCSV language
    return (language, fromJust csv)
  where
    reportAboutMissingCSV lang = die $ "I cannot find .csv-file for language '" <> show lang <> "'."

-- | Loads common food from a single .csv-file and checks it valid.
loadCommonFoodFromOneFile :: FilePath -> IO CompleteFood
loadCommonFoodFromOneFile pathToCSV = do
    commonFood <- Lazy.readFile pathToCSV `catch` possibleProblems
    case extractFood commonFood of
        Left problem -> reportAboutInvalidCSV problem
        Right (capitalize . V.toList -> pairs) -> do
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

    -- Assumed that all food names in .csv-files are in lower case. So we have to
    -- capitalize all these names because of usability: it's more convenient to 
    -- type a name in lowercase on desktop, but mobile keyboard suggests to start 
    -- with uppercase-letter by default. So we must have not only 'orange', but 'Orange' too.
    capitalize :: [(FoodName, CarbPer100g)] -> [(FoodName, CarbPer100g)]
    capitalize lowerCasePairs = concat [ [(capitalized name, carbs), (name, carbs)]
                                       | (name, carbs) <- lowerCasePairs
                                       ]
      where
        capitalized name = (toUpper . T.head $ name) `T.cons` T.tail name

-- | Food entities must be unique and carb values must be between 0.0 and 100.0.
makeSureFoodIsValid :: [(FoodName, CarbPer100g)] -> IO ()
makeSureFoodIsValid food =
    -- Monadic operator '>>', there's no passing values from one function to other.
       makeSureItIsNonEmpty
    >> makeSureNoDuplication
    >> checkCarbValues
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
