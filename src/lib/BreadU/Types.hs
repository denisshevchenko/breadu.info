{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : BreadU.Types
Description : Common types for a library
Stability   : experimental
Portability : POSIX

Common types for a library. It's highly recommended to use
descriptive types, readers of your code will thank you.
-}

module BreadU.Types where

import           TextShow               ( TextShow(..) )
import           Data.Text              ( Text, unpack )
import           Data.HashMap.Strict    ( HashMap )
import           Data.Vector            ( Vector )
import           Data.Aeson             ( ToJSON(..), FromJSON(..) )
import           Web.HttpApiData        ( FromHttpApiData(..) )
import           Web.FormUrlEncoded     ( FromForm(..), toEntriesByKey )
import           GHC.Generics           ( Generic(..) )

type Port = Int

-- | Represents supported languages.
data LangCode = En | De | Ru
    deriving (Eq, Enum)

allLanguages :: [LangCode]
allLanguages = [En .. Ru]

-- | This instance allows us to convert values
-- of 'LangCode' type to the strict 'Text', via 'showt' function.
instance TextShow LangCode where
    showb En = "en"
    showb De = "de"
    showb Ru = "ru"

-- | For work with FilePath.
instance Show LangCode where
    show = unpack . showt

-- | Types for convenient representation of the food info.
type FoodName         = Text
type FoodNamePart     = Text
type CarbPer100g      = Double
type Food             = HashMap FoodName CarbPer100g
type OrderedFoodNames = Vector FoodName
type CompleteFood     = (OrderedFoodNames, Food)
type CompleteFoods    = [(LangCode, CompleteFood)]

-- | Amount of food.
type BU    = Double
type Grams = Double

-- | Type for obtaining of client's language from the request.
-- User will be redirected to language-specific version based on
-- 'Accept-Language' HTTP header's value.
newtype ClientLanguage = ClientLanguage Text

-- | Instance for parsing HTTP header. In this case 
-- we just put header's text in the 'ClientLanguage' type.
instance FromHttpApiData ClientLanguage where
    parseUrlPiece   = Right . ClientLanguage
    parseQueryParam = Right . ClientLanguage

-- | For simplicity names and ids of all food inputs are equal.
type InputName  = Text
type InputId    = Text
type InputValue = Text

-- | Raw food info, just values from all inputs of the main food form.
-- At this point we know nothing about validity of these values.
type RawFoodInfo = [(InputName, [InputValue])]

-- | Type contains values of all inputs from the main food form.
newtype FoodInfoFromForm = FoodInfoFromForm
    { allInputs :: RawFoodInfo
    -- ^ Just all inputs from the form, even with an empty values.
    } deriving (Generic)

-- | Instance we need to extract values of inputs from the main food form.
instance FromForm FoodInfoFromForm where
    fromForm inputs = FoodInfoFromForm <$> toEntriesByKey inputs

-- | Represents one input, with name and (optional) value.
-- In this point we know nothing about the value's validity.
type Input = (InputName, Maybe InputValue)

-- | Type that represents one food item obtained from the food form.
data FoodItem = FoodItem
    { foodName      :: Input -- ^ Raw info from the Food input.
    , carbPer100g   :: Input -- ^ Raw info from the Carbs input.
    , bu            :: Input -- ^ Raw info from the BU input.
    , grams         :: Input -- ^ Raw info from the Grams input.
    } deriving (Show)

-- | Type for prepared food info, for convenient work with it.
newtype FoodInfo = FoodInfo
    { items :: [FoodItem]
    } deriving (Show)

-- | Type for new food item's markup, for AJAX POST response.
newtype NewFood = NewFood
    { itemHTML :: Text
    } deriving (Generic)

-- | For converting 'NewFood' into JSON, in AJAX POST-response.
instance ToJSON NewFood

-- | Localized message for bad input's value (it will be shown as a popover).
type ErrorTitle   = Text
type ErrorMessage = Text
type BadInput     = (InputId, ErrorTitle, ErrorMessage)

-- | Calculated result.
type ResultValue  = Text
type Result       = (InputId, ResultValue)

-- | Type for calculating results. When user clicks to CALCULATE button,
-- AJAX POST-reuest is sending, and we have to calculate food quantity data.
data CalculationResult = CalculationResult
    { badInputs :: [BadInput]   -- ^ Info about inputs with bad values.
    , results   :: [Result]     -- ^ Info about calculated results, for showing in inputs.
    } deriving (Generic)

-- | For converting 'CalculationResult' into JSON, in AJAX POST-response.
instance ToJSON CalculationResult

-- | Type for a food suggestions. When user starts typing food name,
-- AJAX POST-request is sending, and we have to return a few food
-- suggestions.
newtype FoodSuggestions = FoodSuggestions
    { suggestionsHTML :: Text
    } deriving (Generic)
 
-- | For converting 'FoodSuggestions' into JSON, in AJAX POST-response.
instance ToJSON FoodSuggestions

-- | When the user types food name, information about it POST-ed as a value
-- of this type. 'currentURL' will be used to detect user's current language.
data InputedFoodInfo = InputedFoodInfo
    { foodNamePart :: FoodNamePart
    , currentURL   :: Text
    } deriving (Generic)

-- | For converting 'InputedFoodInfo' from JSON, during AJAX POST-request.
instance FromJSON InputedFoodInfo
