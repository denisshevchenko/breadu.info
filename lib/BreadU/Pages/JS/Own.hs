{-# LANGUAGE QuasiQuotes #-}

{-|
Module      : BreadU.Pages.JS.Own
Description : Own JavaScript.
Stability   : experimental
Portability : POSIX

Owr own JavaScript. The core idea is to keep our JavaScript as simple as possible.
For example, we don't build DOM here, but in Haskell side only, because of
compilation guarantees.

I prefer AJAX-requests because of simplicity and efficiency.
-}

module BreadU.Pages.JS.Own
    ( ownJS
    , removeDOMItemBy
    , ajaxPOST
    ) where

import           Text.Jasmine               ( minify )
import           Data.Text                  ( Text )
import           Data.Text.Encoding         ( encodeUtf8, decodeUtf8 )
import           Data.ByteString.Lazy       ( toStrict, fromStrict )
import           Data.String.QQ
import           Data.Monoid                ( (<>) )

-- | Our own JavaScript as a 'Text'.
ownJS :: Text
ownJS = minifyJS [s|
// Submits food form via AJAX POST-request.
function submitFoodForm( langCode ) {
    var foodFormId = "#FoodFormId" + langCode;
    $( foodFormId ).submit(function( event ) {
        event.preventDefault(); // Stop form from submitting normally.
        // Send main food form via AJAX POST-request.
        $.post( langCode + "/calculate", $( foodFormId ).serialize(), function( result ) {
            // Shows calculated results if they're here.
            if ( result.results.length > 0 ) {
                var totalBUArr = jQuery.makeArray( result.results[0] );
                var totalBUId = "#" + totalBUArr[0];
                var totalBUValue = totalBUArr[1];
                $( totalBUId ).delay( 200 ).text( totalBUValue ); 

                $.each( result.results.slice(1), function( i, idAndValue ) {
                    var arr = jQuery.makeArray( idAndValue );
                    var inputId = "#" + arr[0];
                    var value = arr[1];
                    $( inputId ).focus().delay( 200 ).val( value );
                });
            }

            // Shows errors if they're here.
            $.each( result.badInputs, function( i, idAndMessage ) {
                var arr = jQuery.makeArray( idAndMessage );
                var inputId = "#" + arr[0];
                var errorTitle = arr[1];
                var errorMessage = arr[2];
                $( inputId ).popover({ 'placement': 'top', 'title': errorTitle, 'content': errorMessage });
                $( inputId ).popover( 'toggle' );
                
                // Hide popover after user click to this input to correct an invalid value.
                $( inputId ).click(function(e)  { $( inputId ).popover( 'dispose' ); });
                $( inputId ).change(function(e) { $( inputId ).popover( 'dispose' ); });
            });
        });
    });
}

$(document).ready( function() {
    submitFoodForm("en"); // Submits food form, with English-localized results.
    submitFoodForm("ru"); // Submits food form, with Russian-localized results.

    // User began type food name. Send AJAX POST-request for autocomplete suggestions.
    $('body').on('keyup', 'input.FoodInputClass', function() {
        var idOfThisInput = "#" + this.id;
        $.post({
            url: "autocomplete",
            data: $( this ).val(),
            success: function( result ) {
                var obj = JSON.parse( result );
                // Find id of corresponding food input.
                var datalistForThisInput = idOfThisInput + "datalist";
                $( datalistForThisInput ).find('option').remove().end(); // Remove all previous suggestions.
                $( datalistForThisInput ).append( obj.suggestionsHTML ); // Add current suggestions.
            }, 
            dataType: "text",
            contentType: "text/plain; charset=UTF-8"
        });
    });
});
|]

-- | Minify JS.
minifyJS :: Text -> Text
minifyJS = decodeUtf8 . toStrict . minify . fromStrict . encodeUtf8

-- | Remove DOM item by its id, via jQuery remove() method.
removeDOMItemBy :: Text -> Text
removeDOMItemBy anId = "$( \"#" <> anId <> "\" ).remove()"

-- | AJAX POST-request, via jQuery post() method.
-- Callback if OK uses response argument.
ajaxPOST :: Text -> Text -> Text
ajaxPOST url callbackIfOK =
    "$.post({ url: '" <> url <> "', success: function(response) { " <> callbackIfOK <> " } })"
