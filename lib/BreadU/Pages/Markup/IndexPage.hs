{-|
Module      : BreadU.Pages.Markup.IndexPage
Description : HTML markup for the index page.
Stability   : experimental
Portability : POSIX

HTML markup for the index page.
-}

module BreadU.Pages.Markup.IndexPage where

import           BreadU.Types                           ( FoodName )
import           BreadU.Pages.Types                     ( IndexPage(..)
                                                        , IndexBodyContent(..)
                                                        , HeaderContent(..)
                                                        , LangCode(..)
                                                        )
import           BreadU.Pages.CSS.Names                 ( ClassName(..) )
import           BreadU.Pages.JS.Own                    ( removeDOMItemBy, ajaxPOST )
import           BreadU.Pages.Names                     ( ElementName(..) )
import           BreadU.API                             ( indexPageLink
                                                        , addFoodLink
                                                        , calculateFoodLink
                                                        )
import           BreadU.Pages.Content.IndexBody         ( indexBodyContent )
import           BreadU.Pages.Markup.Common.HeadTag     ( commonHeadTag )
import           BreadU.Pages.Markup.Common.Header      ( commonHeader )
import           BreadU.Pages.Markup.Common.Footer      ( commonFooter )
import           BreadU.Pages.Markup.Common.Resources   ( allScripts )
import           BreadU.Pages.Markup.Common.Utils

import           Prelude                                hiding ( div, head, span )
import           Data.Text                              ( Text, unpack, isSuffixOf )
import           Data.Text.Lazy                         ( toStrict )
import           Data.Monoid                            ( (<>) )
import           TextShow                               ( showt )
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes            as A
import           Text.Blaze.Html.Renderer.Text          ( renderHtml )

{-|
   Markup for index page. To serve HTML via Servant we
   should provide 'ToMarkup' instance for 'IndexPage' type.
-}
instance ToMarkup IndexPage where
    -- Render HTML for fake index page.
    toMarkup (RedirectTo Ru) = redirectImmediatelyTo $ indexPageLink Ru
    toMarkup (RedirectTo En) = redirectImmediatelyTo $ indexPageLink En
    
    -- Render markup for the real, localized index page.
    toMarkup IndexPage{..} = do
        -- We're using blaze-html DSL to build HTML. It's building
        -- in monadic context 'Html', so we can compose whole DOM
        -- from the parts in the same context.
        docType
        html ! A.lang (toValue $ showt langCode) $ do
            commonHeadTag (siteTitle headerContentForIndex)
                          (metaDescription headerContentForIndex)
            body $ do
                div ! A.class_ "container" $ do
                    commonHeader headerContentForIndex langCode
                    foodFormBlock langCode bodyContentForIndex
                    commonFooter footerContentForIndex
                allScripts

    preEscapedToMarkup = toMarkup

-- | HTML-based redirection to language-specific index page.
redirectImmediatelyTo :: Text -> Html
redirectImmediatelyTo langSpecificLink = docTypeHtml $ do
    head $ meta ! A.httpEquiv "refresh" ! A.content (toValue $ "0; URL='" <> langSpecificLink <> "'")
    body mempty

-- | Block for the main food form.
foodFormBlock :: LangCode -> IndexBodyContent -> Html
foodFormBlock langCode content@IndexBodyContent{..} = div ! A.class_ (toValue FormBlock) $
    -- Actually this form will be submitted via AJAX POST-request, see JS/Own.hs module.
    form ! A.method "post"
         ! A.id (toValue $ show FoodFormId <> unpack (showt langCode))
         ! A.action (toValue $ calculateFoodLink langCode) $ do
        firstFoodItem content
        totalBUQuantity
        row_ $ do
            div ! A.class_ "col-4 col-xs-5" $ addFoodButton
            div ! A.class_ "col-8 col-xs-7" $ calculateButton
  where
    addFoodButton = div ! A.class_ (toValue AddFood) $
        button ! A.class_ (toValue $ "btn btn-secondary btn-lg btn-block " <> showt AddFoodButton)
               ! A.id (toValue AddFoodButtonId)
               ! A.type_ "button"
               ! A.title (toValue addFoodTitle)
               ! A.onclick (toValue addNewFoodItem) $ do
            span ! A.class_ (toValue MainButtonIcon) $ fa "fa-plus"
            span ! A.class_ (toValue MainButtonIconSeparator) $ mempty
            toHtml addFoodLabel

    calculateButton = div ! A.class_ (toValue Calculate) $
        button ! A.class_ (toValue $ "btn btn-info btn-lg btn-block " <> showt CalculateButton)
               ! A.id (toValue CalculateButtonId)
               ! A.type_ "submit"
               ! A.title (toValue calculateTitle) $ do
            span ! A.class_ (toValue MainButtonIcon) $ fa "fa-calculator"
            span ! A.class_ (toValue MainButtonIconSeparator) $ mempty
            toHtml calculateLabel

    totalBUQuantity = div ! A.class_ (toValue TotalBUQuantity) $ do
        span $ toHtml totalBULabel
        span ! A.id (toValue TotalBUQuantityId) $ "0"

    -- | When user will click to Add button - AJAX POST-request will be sent and new food item will be added.
    addNewFoodItem :: Text
    addNewFoodItem = ajaxPOST (addFoodLink langCode) $ "$('#" <> showt FoodFormFirstItem <> "').append(response.itemHTML)"

{-|
   First food item is always here, always at the top and cannot be removed.
   So we can hardcode id/name for its inputs.
-}
firstFoodItem :: IndexBodyContent -> Html
firstFoodItem IndexBodyContent{..} =
    div ! A.class_ (toValue FoodFormFirstItem)
        ! A.id (toValue FoodFormFirstItem) $
        div ! A.class_ (toValue FoodFormItemInputs) $ do
            row_ $ do
                col_7 $ foodDataInput (showt FirstFoodPrefix <> showt FoodNameInputPostfix) FoodInputClass foodNameLabel
                col_2 $ div ! A.class_ (toValue Or) $ toHtml orAnotherValue
                col_3 $ foodDataInput (showt FirstFoodPrefix <> showt CarbsInputPostfix) CarbsInputClass carbsLabel

            div ! A.class_ (toValue FoodFormRowsSeparator) $ mempty
            
            row_ $ do
                col_5 $ foodDataInput (showt FirstFoodPrefix <> showt BUInputPostfix) BUInputClass buLabel
                col_2 $ div ! A.class_ (toValue Or) $ toHtml orAnotherValue
                col_5 $ foodDataInput (showt FirstFoodPrefix <> showt GramsInputPostfix) GramsInputClass gramsLabel

-- | Return 'Text' with rendered HTML. It will be sent as a response to AJAX POST-request
-- after user clicked to Add new food in calculation.
newFoodItem :: LangCode -> Text -> Text
newFoodItem langCode idPrefix = toStrict . renderHtml $
    newFoodItemCommon (indexBodyContent langCode) idPrefix

-- | New food item, language-agnostic variant.
newFoodItemCommon :: IndexBodyContent -> Text -> Html
newFoodItemCommon IndexBodyContent{..} idPrefix =
    div ! A.class_ (toValue FoodFormItem)
        ! A.id (toValue thisFoodItemId) $ do
        div ! A.class_ (toValue FoodFormItemInputs) $ do
            row_ $ do
                col_7 $ foodDataInput (idPrefix <> showt FoodNameInputPostfix) FoodInputClass foodNameLabel
                col_2 $ div ! A.class_ (toValue Or) $ toHtml orAnotherValue
                col_3 $ foodDataInput (idPrefix <> showt CarbsInputPostfix) CarbsInputClass carbsLabel

            div ! A.class_ (toValue FoodFormRowsSeparator) $ mempty
            
            row_ $ do
                col_5 $ foodDataInput (idPrefix <> showt BUInputPostfix) BUInputClass buLabel
                col_2 $ div ! A.class_ (toValue Or) $ toHtml orAnotherValue
                col_5 $ foodDataInput (idPrefix <> showt GramsInputPostfix) GramsInputClass gramsLabel
            
        div ! A.class_ (toValue RemoveIconFoodForm) $
            a ! A.onclick (toValue $ removeDOMItemBy thisFoodItemId)
              ! A.title (toValue removeFoodItemTitle) $
                preEscapedToMarkup ("&times;" :: String)
  where
    thisFoodItemId :: Text
    thisFoodItemId = idPrefix <> showt FoodFormItemId

data AutoFocus = AutoFocus | NoAutoFocus

-- | One input for food-related value.
-- For simplicity 'commonName' is using both for name and id attributes.
foodDataInput :: Text -> ClassName -> Text -> Html
foodDataInput commonName additionalClass aLabel = div ! A.class_ "md-form" $ do
    input ! A.type_ "text"
          ! A.class_ (toValue $ "form-control " <> showt additionalClass)
          ! foodInputDatalistIdIfRequired
          ! A.autocomplete "off" -- We don't need browser's default autocomplete feature.
          ! A.id (toValue commonName)
          ! A.name (toValue commonName)
    label ! A.for (toValue commonName) $ toHtml aLabel
    foodInputDatalistIfRequired
  where
    thisIsFoodInput = showt FoodNameInputPostfix `isSuffixOf` commonName
    
    foodInputDatalistIdIfRequired =
        if thisIsFoodInput then A.list (toValue datalistId) else mempty

    foodInputDatalistIfRequired =
        if thisIsFoodInput then datalist ! A.id (toValue datalistId) $ "" else mempty
    
    datalistId = commonName <> "datalist"

-- | Options for datalist with a food suggestions.
-- Again, we render this small HTML here, not in JavaScript.
optionsForDatalist :: [FoodName] -> Text
optionsForDatalist = toStrict . renderHtml . mapM_ addOption
  where
    addOption :: FoodName -> Html
    addOption suggestion = option ! A.value (toValue suggestion) $ mempty
