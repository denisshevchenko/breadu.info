{-|
Module      : CLI
Description : CLI options
Stability   : experimental
Portability : POSIX

Work with CLI options.
-}

module CLI
    ( optionsParser
    , makeSureOptionsAreValid
    ) where

import           Options.Applicative.Simple
import           Data.Monoid                    ( (<>) )
import           Control.Monad                  ( when )
import           Control.Monad.Extra            ( unlessM )
import           System.Directory               ( doesFileExist )
import           System.Exit                    ( die )

-- | Type that represents CLI options, we use it just for 'Parser'.
data Options = Options
    { commonFood :: FilePath -- ^ Path to .csv-file with common list of food.
    , port       :: Int      -- ^ Port that server will listen.
    }

-- | Parser parses actual CLI-arguments into a value of 'Option' type.
optionsParser :: Parser Options
optionsParser = Options
      <$> strOption (
            long        "food"
         <> short       'f'
         <> metavar     "PATH_TO_CSV"
         <> showDefault
         <> value       "./food/common.csv" -- Option's default value.
         <> help        "Path to .csv-file with common list of food"
      )
      <*> option auto (
            long        "port"
         <> short       'p'
         <> metavar     "PORT"
         <> showDefault
         <> value       3000                -- Option's default value.
         <> help        "Port that server will listen"
      )

{-|
   Just checks if options are valid, exit with error otherwise.
   Note that we have a tuple as a final result, not an 'Options' type,
   because we don't want unnecessary dependencies outside this module.
-}
makeSureOptionsAreValid :: (Options, ()) -> IO (FilePath, Int)
makeSureOptionsAreValid (Options {..}, ()) =
       makeSurePortIsValid
    >> makeSureCSVExists
    >> return (commonFood, port)
  where
    -- | Checks if specified value is valid registered port,
    -- please see https://en.wikipedia.org/wiki/Registered_port for details.
    makeSurePortIsValid :: IO ()
    makeSurePortIsValid =
        when (port < minPort || port > maxPort) reportAboutWrongPort
      where
        minPort = 1024  -- There's no reasons to run this service on a privileged port. ;-)
        maxPort = 49151
        reportAboutWrongPort = die $
            "Please specify valid registered port, integer from "
            <> show minPort <> " to " <> show maxPort <> "."

    makeSureCSVExists :: IO ()
    makeSureCSVExists =
        unlessM (doesFileExist commonFood) reportAboutCSVMissing
      where
        reportAboutCSVMissing = die $
            "No such file '" <> commonFood <> "', you can specify path to .csv-file via '--food' option."
