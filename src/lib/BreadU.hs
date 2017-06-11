{-|
Module      : BreadU
Description : Exposed module of 'breadu' library
Stability   : experimental
Portability : POSIX

Exposed module of 'breadu' library. 'breadu-exe' app uses this module to run a server,
please see "BreadU.Server" module for details.
-}

module BreadU
    ( startBreadU
    ) where

import           BreadU.Types               ( CompleteFoods, Port )
import           BreadU.Tools.CommonFood    ( loadCommonFood )
import           BreadU.API                 ( api )
import           BreadU.Server              ( server )

import           Servant                    ( serve )
import           Network.Wai                ( Application )
import           Network.Wai.Handler.Warp   ( run )

-- | Runs a server on specified port. After start we read common food from the .csv-files.
startBreadU :: (FilePath, Port) -> IO ()
startBreadU (pathToCSVDir, port) = loadCommonFood pathToCSVDir >>= run port . app
  where
    app :: CompleteFoods -> Application
    app = serve api . server
