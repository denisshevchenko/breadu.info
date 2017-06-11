{-|
Module      : BreadU.Pages.Content.Header
Description : Localized content for page's header.
Stability   : experimental
Portability : POSIX

Localized content for page's header.
Fields of type 'HeaderContent' will be used in pages' markup.
-}

module BreadU.Pages.Content.Header
    ( headerContent
    ) where

import           BreadU.Types           ( LangCode(..) )
import           BreadU.Pages.Types     ( HeaderContent(..) )

-- | Localized content for page's header .
headerContent :: LangCode -> HeaderContent
headerContent Ru = HeaderContent
    { metaDescription     = "Калькулятор хлебных единиц (ХЕ) для диабетиков. Переводит ХЕ в граммы и наоборот, считает ХЕ для набора продуктов."
    , aboutLabel          = "Что это"
    , aboutTitle          = "О сервисе"
    , siteName            = "Хлебная Единица"
    , siteTitle           = "Калькулятор хлебных единиц"
    , briefDescription    = "Калькулятор хлебных единиц для диабетиков."
    , buDescription       = "1 хлебная единица (ХЕ) соответствует количеству продукта, содержащего 10-12 грамм усваиваемых углеводов."
    , howToUseItTitle     = "Как этим пользоваться"
    , howToUseItFood      = "В поле «Продукт» введите название продукта. Начните печатать, и вам будет предложено выбрать из нескольких подходящих вариантов. Если же среди предложенных вариантов нет вашего продукта, вы можете указать его углеводное значение — количество углеводов на 100 г продукта — в поле «Углеводы»."
    , howToUseItQuantity  = "Затем введите количество продукта, либо в ХЕ, либо в граммах."
    , howToUseItAddFood   = "Нажав кнопку «+», вы можете добавить ещё один продукт, их количество неограничено. Для каждого из добавленных продуктов укажите название/углеводное значение, а также количество. Вы можете удалить любой из добавленных продуктов, нажав на крестик в верхнем правом углу."
    , howToUseItCalculate = "Нажмите кнопку «ПОСЧИТАТЬ». Соответствующие значения ХЕ/граммов для каждого из продуктов будут рассчитаны автоматически. Также вы увидите общее количество ХЕ."
    , disclaimerTitle     = "Внимание"
    , disclaimer          = "Я очень рад, что мой калькулятор помогает людям, живущим с сахарным диабетом, и я сам постоянно пользуюсь этим калькулятором. Однако я не являюсь эндокринологом, не имею медицинского образования, и этот калькулятор не может рассматриваться как официальное медицинское руководство по питанию для диабетиков. Если вам нужна медицинская консультация — пожалуйста, обратитесь к специалисту."
    , ossTitle            = "Свободная программа"
    -- Please note that if you're using raw html-tags in the text,
    -- you should convert it not with 'toHtml' function, but with 'preEscapedToHtml' function.
    , oss                 = "Этот калькулятор является свободным программным обеспечением. Вы можете бесплатно пользоваться им без ограничений по времени, в соответствии с условиями <a href=\"https://github.com/denisshevchenko/breadu.info/blob/master/LICENSE\">лицензии MIT</a>. Исходный код калькулятора открыт и доступен на <a href=\"https://github.com/denisshevchenko/breadu.info\">GitHub</a>."
    , githubTitle         = "Исходный код на GitHub"
    }
headerContent En = HeaderContent
    { metaDescription     = "Bread units (BU) calculator for diabetics. It converts BU to grams and vice versa, calculates BU for the custom food set."
    , aboutLabel          = "About"
    , aboutTitle          = "About a service"
    , siteName            = "Bread Unit"
    , siteTitle           = "Bread unit calculator"
    , briefDescription    = "Bread units calculator for diabetics."
    , buDescription       = "1 bread unit (BU) corresponds to quantity of food that contains 10-12 grams of digestible carbohydrates."
    , howToUseItTitle     = "How to use it"
    , howToUseItFood      = "Enter food name into the «Food name» field. Start typing and you will see some suggestions. If no suggestion appears, you can enter the product's carbohydrates value — quantity of carbohydrates per 100g — into the «Carbs» field."
    , howToUseItQuantity  = "Then enter food quantity. You can specify it in BUs or in grams."
    , howToUseItAddFood   = "You can add a new product by clicking on the «+» button. For all added products enter name/carbohydrates value and quantity. You can delete added product by clicking on the cross in the upper right corner."
    , howToUseItCalculate = "Now click on the «CALCULATE» button. Corresponding values of BU/grams for each product will be calculated automatically. You will see the total BU value as well."
    , disclaimerTitle     = "Attention"
    , disclaimer          = "I'm so glad that this calculator helps people who live with diabetes mellitus. Personally I use it every day. But I'm not an endocrinologist and have no medical education, so this calculator cannot be treated as an official medical guide to nutrition for diabetics. If you need medical advice, please consult a specialist."
    , ossTitle            = "Free software"
    , oss                 = "This calculator is free. You may use it without time limitation in accordance with the terms of <a href=\"https://github.com/denisshevchenko/breadu.info/blob/master/LICENSE\">MIT license</a>. The calculator's source code is open and available on <a href=\"https://github.com/denisshevchenko/breadu.info\">GitHub</a>."
    , githubTitle         = "Source code on GitHub"
    }
headerContent De = HeaderContent
    { metaDescription     = "Broteinheiten (BE) Rechner für Diabetiker. Er wandelt BE in Gramm um (sowie umgekehrt) und berechnet BE für spezielle Lebensmittel."
    , aboutLabel          = "Über"
    , aboutTitle          = "Über den Service"
    , siteName            = "Broteinheiten"
    , siteTitle           = "Broteinheiten Rechner"
    , briefDescription    = "Broteinheiten Rechner für Diabetiker."
    , buDescription       = "1 Broteinheit (BE) entspricht einer Menge an Lebensmitteln, welche 10-12 Gramm verdauliche Kohlenhydrate beinhaltet."
    , howToUseItTitle     = "Anleitung"
    , howToUseItFood      = "Geben Sie ein Lebensmittel in das «Lebensmittel» Feld ein. Bei Eingabe des Namens werden Sie einige passende Vorschläge sehen. Falls das Lebensmittel nicht aufgelistet ist, können Sie stattdessen auch den Wert der Kohlenhydrate — Menge an Kohlenhydrate per 100g — in das Feld «Kohlenhydrate» eingeben."
    , howToUseItQuantity  = "Geben Sie danach die Menge des Lebensmittels ein. Sie können die Angabe in BE oder in Gramm vornehmen."
    , howToUseItAddFood   = "Sie können ein neues Lebensmittel über den «Hinzufügen» Button anlegen. Geben Sie für jedes Lebensmittel den Namen bzw. die Menge der Kohlenhydrate an. Sie können jedes Lebensmittel durch Anklicken auf das rechte, obere Kreuz wieder entfernen."
    , howToUseItCalculate = "Klicken Sie nun auf den «BERECHNEN» Button. Die entsprechenden Werte von BE/Gramm werden für jedes Lebensmittel automatisch berechnet. Ausserdem werden sie die Gesamtmenge an BE sehen."
    , disclaimerTitle     = "Achtung"
    , disclaimer          = "Ich freue mich sehr, dass dieser Rechner Menschen hilft, welche mit Diabetes mellitus leben. Auch ich benutze ihn jeden Tag. Dennoch bin ich kein Endokrinologe und habe keine medizinische Ausbildung, so dass dieser Rechner nicht als offizielle, medizinische Anleitung für eine Ernährung von Diabetikern betrachtet werden kann. Falls Sie eine medizinische Beratung benötigen, konsultieren Sie bitte einen Spezialisten."
    , ossTitle            = "Freie Software"
    , oss                 = "Dieser Rechner ist eine freie Software. Sie können diesen kostenlos und ohne zeitliche Einschränkung nutzen, unter Berücksichtigung <a href=\"https://github.com/denisshevchenko/breadu.info/blob/master/LICENSE\">MIT Lizenz</a>. Der Source Code für den Rechner ist frei verfügbar unter <a href=\"https://github.com/denisshevchenko/breadu.info\">GitHub</a>."
    , githubTitle         = "Source Code bei GitHub"
    }
