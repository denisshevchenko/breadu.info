{-|
Module      : BreadU.Pages.CSS.Names
Description : Names for our own CSS-classes.
Stability   : experimental
Portability : POSIX

Names for our own CSS-classes. We don't want to work with raw string literals explicitly,
so we just define a type with nullary constructors corresponding to names of classes.
These constructors can be represented as a 'Text', so we'll use it in our own CSS
as well as in HTML markup.

The only classes we use as a raw string literals are third-party classes, for example from Bootstrap library.
-}

module BreadU.Pages.CSS.Names
    ( ClassName(..) 
    ) where

import           Text.Blaze     ( ToValue(..) )
import           TextShow       ( TextShow(..), fromText )
import           Data.Text      ( pack )

-- | Type for all our own CSS-classes.
data ClassName
    = LanguageSwitcher
    | LanguageSwitcherDelimiter
    | AboutInfo
    | CurrentLanguage
    | FormBlock
    | FoodFormRowsSeparator
    | FoodFormFirstItem
    | FoodFormItem
    | FoodFormItemInputs
    | FoodItemsSeparator
    | FoodNameInfo
    | FoodAmountInfo
    | FoodInputClass
    | CarbsInputClass
    | BUInputClass
    | GramsInputClass
    | Or
    | TotalBUQuantity
    | AddFood
    | AddFoodButton
    | Calculate
    | CalculateButton
    | MainButtonIcon
    | MainButtonIconSeparator
    | InfoIconFoodForm
    | RemoveIconFoodForm
    | AuthorInfo
    | AuthorInfoMailToSeparator
    | MailToIcon
    | SocialButtons
    | SocialButtonsSeparator
    | Block404
    | Block404Mark
    | Block404Mark0
    | Block404Description
    deriving (Show)

-- | We want to use constructors as attributes' values in HTML markup.
instance ToValue ClassName where
    toValue = toValue . show

{-|
   We want 'Text'-representation of all constructors, but it's impossible
   to derive from 'TextShow' class explicitly. So we use standard 'show'
   to convert 'String'-representations of constructors into 'Text'-representations.
-}
instance TextShow ClassName where
    showb = fromText . pack . show
