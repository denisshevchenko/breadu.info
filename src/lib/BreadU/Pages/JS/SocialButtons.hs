{-# LANGUAGE QuasiQuotes #-}

{-|
Module      : BreadU.Pages.JS.SocialButtons
Description : SocialButtons JavaScript.
Stability   : experimental
Portability : POSIX

Third-party JS for social buttons.
-}

module BreadU.Pages.JS.SocialButtons
    ( twitterWidget
    , facebookSDK
    ) where

import           BreadU.Types       ( LangCode(..) )

import           Data.Text          ( Text )
import           Data.String.QQ

-- | Official Twitter widget, it's using for the Tweet button. 
twitterWidget :: Text
twitterWidget = [s|
window.twttr = (function(d, s, id) {
  var js, fjs = d.getElementsByTagName(s)[0],
    t = window.twttr || {};
  if (d.getElementById(id)) return t;
  js = d.createElement(s);
  js.id = id;
  js.src = "https://platform.twitter.com/widgets.js";
  fjs.parentNode.insertBefore(js, fjs);

  t._e = [];
  t.ready = function(f) {
    t._e.push(f);
  };

  return t;
}(document, "script", "twitter-wjs"));
|]

-- | Facebook SDK, with app's id.
facebookSDK :: LangCode -> Text
facebookSDK En = [s|
(function(d, s, id) {
  var js, fjs = d.getElementsByTagName(s)[0];
  if (d.getElementById(id)) return;
  js = d.createElement(s); js.id = id;
  js.src = "//connect.facebook.net/en_US/sdk.js#xfbml=1&version=v2.9&appId=1162939683834190";
  fjs.parentNode.insertBefore(js, fjs);
}(document, 'script', 'facebook-jssdk'));
|]
facebookSDK De = [s|
(function(d, s, id) {
  var js, fjs = d.getElementsByTagName(s)[0];
  if (d.getElementById(id)) return;
  js = d.createElement(s); js.id = id;
  js.src = "//connect.facebook.net/de_DE/sdk.js#xfbml=1&version=v2.9&appId=1162939683834190";
  fjs.parentNode.insertBefore(js, fjs);
}(document, 'script', 'facebook-jssdk'));
|]
facebookSDK Ru = [s|
(function(d, s, id) {
  var js, fjs = d.getElementsByTagName(s)[0];
  if (d.getElementById(id)) return;
  js = d.createElement(s); js.id = id;
  js.src = "//connect.facebook.net/ru_RU/sdk.js#xfbml=1&version=v2.9&appId=1162939683834190";
  fjs.parentNode.insertBefore(js, fjs);
}(document, 'script', 'facebook-jssdk'));
|]
