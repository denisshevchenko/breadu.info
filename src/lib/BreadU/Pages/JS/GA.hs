{-# LANGUAGE QuasiQuotes #-}

{-|
Module      : BreadU.Pages.JS.GA
Description : GA JavaScript.
Stability   : experimental
Portability : POSIX

JS for Google Analytics.
-}

module BreadU.Pages.JS.GA
    ( googleAnalytics
    ) where

import           Data.Text                  ( Text )
import           Data.String.QQ

-- | Official Twitter widget, it's using for the Tweet button. 
googleAnalytics :: Text
googleAnalytics = [s|
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-28253221-8', 'auto');
  ga('send', 'pageview');
|]
