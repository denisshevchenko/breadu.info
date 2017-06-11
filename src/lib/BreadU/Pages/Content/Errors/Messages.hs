{-|
Module      : BreadU.Pages.Content.Errors.Messages
Description : Localized error messages.
Stability   : experimental
Portability : POSIX

Localized error messages. These messages will be shown to the user
near corresponding form's inputs if some values are incorrect.
-}

module BreadU.Pages.Content.Errors.Messages where

import           BreadU.Types           ( CarbPer100g, LangCode(..) )

import           TextShow               ( showt )
import           Data.Monoid            ( (<>) )
import           Data.Text              ( Text )

missedFoodErrorTitle :: LangCode -> Text
missedFoodErrorTitle Ru = "Продукт не задан"
missedFoodErrorTitle En = "Food isn't defined"
missedFoodErrorTitle De = "Lebensmittel wurde nicht angegeben"

missedFoodErrorMessage :: LangCode -> Text
missedFoodErrorMessage Ru = "Пожалуйста, введите здесь название продукта или его углеводное значение в поле справа."
missedFoodErrorMessage En = "Please enter the food name in this input or its carbohydrates value in the next input."
missedFoodErrorMessage De = "Bitte geben Sie den Namen des Lebensmittels in dieses Eingeabefeld oder den Wert der Kohlenhydrate in das nächste Eingeabefeld."

missedQuantityErrorTitle :: LangCode -> Text
missedQuantityErrorTitle Ru = "Количество не задано"
missedQuantityErrorTitle En = "Quantity isn't defined"
missedQuantityErrorTitle De = "Menge wurde nicht angeben"

missedQuantityErrorMessage :: LangCode -> Text
missedQuantityErrorMessage Ru = "Пожалуйста, введите здесь количество продукта в ХЕ или в граммах в поле справа."
missedQuantityErrorMessage En = "Please enter the food quantity in BU in this input or in grams in the next input."
missedQuantityErrorMessage De = "Bitte geben Sie die Menge des Lebensmittels in BE in dieses Eingabefeld oder in Gramm in das nächste Eingabefeld ein."

carbIncorrectNumberErrorTitle :: LangCode -> Text
carbIncorrectNumberErrorTitle Ru = "Что-то не так с углеводами"
carbIncorrectNumberErrorTitle En = "Something wrong with carbs"
carbIncorrectNumberErrorTitle De = "Etwas ist falsch mit der Angabe für die Kohlenhydrate"

carbIncorrectNumberErrorMessage :: LangCode -> CarbPer100g -> CarbPer100g -> Text
carbIncorrectNumberErrorMessage Ru minCarbs maxCarbs = "Пожалуйста, введите углеводное значение целым или десятичным числом между "
                                                       <> showt minCarbs <> " и " <> showt maxCarbs <> "."
carbIncorrectNumberErrorMessage En minCarbs maxCarbs = "Please enter carbohydrates value as an integer (or decimal) value between "
                                                       <> showt minCarbs <> " and " <> showt maxCarbs <> "."
carbIncorrectNumberErrorMessage De minCarbs maxCarbs = " Bitte geben Sie den Wert der Kohlenhydrate als ganze Zahl (oder Dezimalzahl) zwischen "
                                                       <> showt minCarbs <> " und " <> showt maxCarbs <> " an."

buIncorrectNumberErrorTitle :: LangCode -> Text
buIncorrectNumberErrorTitle Ru = "Что-то не так с ХЕ"
buIncorrectNumberErrorTitle En = "Something wrong with BU"
buIncorrectNumberErrorTitle De = "Etwas ist falsch mit der Angabe für BE"

buIncorrectNumberErrorMessage :: LangCode -> Text
buIncorrectNumberErrorMessage Ru = "Пожалуйста, введите количество ХЕ целым или десятичным числом."
buIncorrectNumberErrorMessage En = "Please enter BU value as an integer (or decimal) number."
buIncorrectNumberErrorMessage De = "Bitte geben Sie den BE Wert als ganze Zahl (oder Dezimalzahl) ein."

gramsIncorrectNumberErrorTitle :: LangCode -> Text
gramsIncorrectNumberErrorTitle Ru = "Что-то не так с граммами"
gramsIncorrectNumberErrorTitle En = "Something wrong with grams"
gramsIncorrectNumberErrorTitle De = "Etwas ist falsch mit der Angabe für Gramm"

gramsIncorrectNumberErrorMessage :: LangCode -> Text
gramsIncorrectNumberErrorMessage Ru = "Пожалуйста, введите количество граммов целым или десятичным числом."
gramsIncorrectNumberErrorMessage En = "Please enter grams as an integer (or decimal) number."
gramsIncorrectNumberErrorMessage De = "Bitte geben Sie den Wert für Gramm als ganze Zahl (oder Dezimalzahl) ein."

foodUnknownNameErrorTitle :: LangCode -> Text
foodUnknownNameErrorTitle Ru = "Неизвестный продукт"
foodUnknownNameErrorTitle En = "Unknown food"
foodUnknownNameErrorTitle De = "Unbekanntes Lebensmittel"

foodUnknownNameErrorMessage :: LangCode -> Text
foodUnknownNameErrorMessage Ru = "Пожалуйста, выберите продукт из предлагаемых вариантов или укажите его углеводное значение в поле справа."
foodUnknownNameErrorMessage En = "Please select the food from offered list or enter carbohydrates value in the next input."
foodUnknownNameErrorMessage De = "Bitte wählen Sie das Lebensmittel von der Liste aus oder geben Sie den Wert der Kohlenhydrate in das nächste Eingabefeld."
