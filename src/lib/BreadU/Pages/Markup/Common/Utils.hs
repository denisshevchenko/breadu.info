{-|
Module      : BreadU.Pages.Markup.Common.Utils
Description : HTML markup utils.
Stability   : experimental
Portability : POSIX

HTML markup utils.
-}

module BreadU.Pages.Markup.Common.Utils where

import           Prelude                        hiding ( span, div )
import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text )
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes    as A

-- | Row for Bootstrap grid. Partially-applied function,
-- it has to be applied to an 'Html'-expression.
row_ :: Html -> Html
row_ = div ! A.class_ "row"

-- | Columns for Bootstrap grid. Partially-applied function,
-- it has to be applied to an 'Html'-expression.
col_1
  , col_2
  , col_3
  , col_4
  , col_5
  , col_6
  , col_7
  , col_8
  , col_9
  , col_10
  , col_11
  , col_12 :: Html -> Html
col_1  = col_ 1
col_2  = col_ 2
col_3  = col_ 3
col_4  = col_ 4
col_5  = col_ 5
col_6  = col_ 6
col_7  = col_ 7
col_8  = col_ 8
col_9  = col_ 9
col_10 = col_ 10
col_11 = col_ 11
col_12 = col_ 12

-- | Column for Bootstrap grid. Partially-applied function,
-- it has to be applied to an 'Html'-expression.
col_ :: Int -> Html -> Html
col_ width = div ! A.class_ (toValue $ "col-" <> show width)

-- | Font Awesome icon.
fa :: Text -> Html
fa iconName = i ! A.class_ (toValue $ "fa " <> iconName)
                ! customAttribute "aria-hidden" "true" $ mempty
