{-|
Module      : BreadU.Pages.Names
Description : Names for pages' elements.
Stability   : experimental
Portability : POSIX

Names for pages' elements. These names will be used in HTML markup as well as
in handlers (thus, we'll extract actual values from the forms based on these names).
-}

module BreadU.Pages.Names
    ( ElementName(..)
    ) where

import           Text.Blaze     ( ToValue(..) )
import           TextShow       ( TextShow(..), fromText )
import           Data.Text      ( pack )

{-|
   We don't want to work with string literals explicitly, so we just
   define a type with nullary constructors corresponding to using ids and names.
-}
data ElementName
    = FoodFormId
    | FirstFoodPrefix
    | FoodNameInputPostfix
    | CarbsInputPostfix
    | BUInputPostfix
    | GramsInputPostfix
    | RemoveFoodButtonPostfix
    | AddFoodButtonId
    | CalculateButtonId
    | FoodFormItemId
    deriving (Eq, Show, Read)

-- | We want to use constructors as attributes' values in HTML markup.
instance ToValue ElementName where
    toValue = toValue . show

{-|
   We want 'Text'-representation of all constructors, but it's impossible
   to derive from 'TextShow' class explicitly. So we use standard 'show'
   to convert 'String'-representations of constructors into 'Text'-representations.
-}
instance TextShow ElementName where
    showb = fromText . pack . show
