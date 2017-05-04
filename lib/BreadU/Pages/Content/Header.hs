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

import           BreadU.Pages.Types     ( LangCode(..), HeaderContent(..) )

-- | Localized content for page's header .
headerContent :: LangCode -> HeaderContent
headerContent Ru = HeaderContent
    { metaDescription     = "Калькулятор хлебных единиц (ХЕ) для диабетиков. Переводит ХЕ в граммы и наоборот, считает ХЕ для набора продуктов."
    , switchToRuTitle     = "Русский"
    , switchToEnTitle     = "Английский"
    , aboutLabel          = "Что это"
    , aboutTitle          = "О сервисе"
    , siteName            = "Хлебная Единица"
    , siteTitle           = "Калькулятор хлебных единиц"
    , briefDescription    = "Калькулятор хлебных единиц для диабетиков."
    , buDescription       = "1 хлебная единица (ХЕ) соответствует количеству продукта, содержащего 10~12 грамм усваиваемых углеводов."
    , howToUseItTitle     = "Как этим пользоваться"
    , howToUseItFood      = "В поле «Название продукта» введите название. Начните печатать, и вам будет предложено выбрать из нескольких подходящих вариантов. Если же среди предложенных вариантов нет вашего продукта, вы можете указать его углеводное значение — количество углеводов на 100 г продукта — в поле «Углеводы»."
    , howToUseItQuantity  = "Затем введите количество продукта, либо в ХЕ, либо в граммах."
    , howToUseItAddFood   = "Нажав кнопку «ЕЩЁ», вы можете добавить ещё один продукт, их количество неограничено. Для каждого из добавленных продуктов укажите название/углеводное значение, а также количество. Вы можете удалить любой из добавленных продуктов, нажав на крестик в верхнем правом углу."
    , howToUseItCalculate = "Нажмите кнопку «ПОСЧИТАТЬ». Соответствующие значения ХЕ/граммов для каждого из продуктов будут рассчитаны автоматически."
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
    , switchToRuTitle     = "Russian"
    , switchToEnTitle     = "English"
    , aboutLabel          = "About"
    , aboutTitle          = "About a service"
    , siteName            = "Bread Unit"
    , siteTitle           = "Bread unit calculator"
    , briefDescription    = "Bread units calculator for diabetics."
    , buDescription       = "1 bread unit (BU) corresponds to a quantity of food that contains 10~12 grams of digestible carbohydrates."
    , howToUseItTitle     = "How to use it"
    , howToUseItFood      = "Enter food name in the «Food name» field. Start typing and you'll see a few corresponding suggestions. If there's no your food, you can enter its carbohydrates value — quantity of carbohydrates per 100g — in the «Carbs» field."
    , howToUseItQuantity  = "Then enter food quantity. You can specify it in BU or in grams."
    , howToUseItAddFood   = "You can add a new product by clicking to «ADD» button. For all added products enter name/carbohydrates value and quantity. You can delete added product by clicking on a cross in the upper right corner."
    , howToUseItCalculate = "Now click to «CALCULATE» button. Corresponding values of BU/grams for each product will be calculated automatically."
    , disclaimerTitle     = "Attention"
    , disclaimer          = "I'm so glad that this calculator helps people who live with diabetes mellitus. And I use it every day. But I'm not an endocrinologist and haven't medical education, do this calculator cannot be treated as an official medical guide to nutrition for diabetics. If you need a medical consultation — please consult a specialist."
    , ossTitle            = "Free software"
    , oss                 = "This calculator is a free software. You may use it for free and without time limitation, in accordance with terms of <a href=\"https://github.com/denisshevchenko/breadu.info/blob/master/LICENSE\">MIT license</a>. Calculator's source code is open and available on <a href=\"https://github.com/denisshevchenko/breadu.info\">GitHub</a>."
    , githubTitle         = "Source code on GitHub"
    }
