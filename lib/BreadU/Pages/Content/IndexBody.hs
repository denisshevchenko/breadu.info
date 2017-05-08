{-|
Module      : BreadU.Pages.Content.IndexBody
Description : Localized content for index page's body.
Stability   : experimental
Portability : POSIX

Localized content for index page's body.
Fields of type 'IndexBodyContent' will be used in index page's markup.
-}

module BreadU.Pages.Content.IndexBody
    ( indexBodyContent
    ) where

import           BreadU.Pages.Types     ( LangCode(..), IndexBodyContent(..) )

-- | Localized content for index page's body.
indexBodyContent :: LangCode -> IndexBodyContent
indexBodyContent Ru = IndexBodyContent
    { foodNameLabel       = "Продукт"
    , carbsLabel          = "Углеводы"
    , buLabel             = "в ХЕ"
    , gramsLabel          = "в граммах"
    , totalBULabel        = "Всего ХЕ: "
    , addFoodLabel        = "Ещё"
    , addFoodTitle        = "Добавить ещё один продукт для расчёта"
    , calculateLabel      = "Посчитать"
    , calculateTitle      = "Посчитать граммы и ХЕ"
    , removeFoodItemTitle = "Удалить этот продукт из расчёта"
    , orAnotherValue      = "или"
    }
indexBodyContent En = IndexBodyContent
    { foodNameLabel       = "Food name"
    , carbsLabel          = "Carbs"
    , buLabel             = "in BU"
    , gramsLabel          = "in grams"
    , totalBULabel        = "Total BU: "
    , addFoodLabel        = "Add"
    , addFoodTitle        = "Add one more food to calculate"
    , calculateLabel      = "Calculate"
    , calculateTitle      = "Calculate grams and BU"
    , removeFoodItemTitle = "Delete this food from calculation"
    , orAnotherValue      = "or"
    }
indexBodyContent De = IndexBodyContent
    { foodNameLabel       = "Lebensmittel"
    , carbsLabel          = "Kohlenhydrate"
    , buLabel             = "in Brotgerät"
    , gramsLabel          = "in Gramm"
    , totalBULabel        = "Gesamtzahl BG: "
    , addFoodLabel        = "Hinzufügen"
    , addFoodTitle        = "Füge noch ein Lebensmittel hinzu"
    , calculateLabel      = "Berechnen"
    , calculateTitle      = "Berechnen Sie Gramm und BG"
    , removeFoodItemTitle = "Löschen Sie dieses Lebensmittel aus der Berechnung"
    , orAnotherValue      = "oder"
    }
