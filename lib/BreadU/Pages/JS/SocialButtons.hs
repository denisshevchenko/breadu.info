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
    ) where

import           Data.Text                  ( Text )
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
