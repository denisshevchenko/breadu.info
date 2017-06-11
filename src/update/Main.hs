{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Main
Description : Main module for 'update-breadu-server' program.
Stability   : experimental
Portability : POSIX

This program updates 'breadu-exe' server. Assumed that:

1. New executable, common food and static files already copied here from the CI-server.
2. Current user can do sudo-commands without password, for simplicity.
3. Nginx web server is already installed on production server.
4. Nginx config /etc/nginx/sites-enabled/default already contains 'proxy_pass'-record for 127.0.0.1:3000.
-}

module Main where

import           Data.Conduit.Shell
import           Data.Conduit.Shell.Segments    ( strings )
import           System.FilePath.Posix          ( (</>) )
import           Control.Applicative            ( (<|>) )
import           Data.Monoid                    ( (<>) )

-- | Helper types.
type Port     = String
type PID      = String
data FromPort = From Port
data ToPort   = To Port

main :: IO ()
main = run $ do
    mv pathToTmpExecutable pathToExecutable
    lookPIDOfCurrentProcess >>= \case
        Nothing  -> startNewServerOn defaultPort
        Just pid -> updateServer pid
  where
    lookPIDOfCurrentProcess :: Segment (Maybe PID)
    lookPIDOfCurrentProcess = strings (   pgrep "-l" executableName <|> return ()
                                       $| awk "{print $1}")
                              >>= \output ->
        return $ if null output then Nothing else Just $ head output

    updateServer :: PID -> Segment ()
    updateServer pidOfCurrentProcess = 
        strings (   sudo "netstat" "-antulp"  -- There's no operator to combine 'sudo' with command... :-(
                 $| grep executableName
                 $| awk "{split($4,a,\":\"); print a[2]}") >>= \(head -> currentPort) -> do
            if currentPort == defaultPort
                then do
                    startNewServerOn alternativePort
                    switchNginxToNewServer (From defaultPort) (To alternativePort)
                else do
                    startNewServerOn defaultPort
                    switchNginxToNewServer (From alternativePort) (To defaultPort)
            stopOldServerBy pidOfCurrentProcess
 
    startNewServerOn :: Port -> Segment ()
    startNewServerOn port = startStopDaemon "--start" "-b" "-q" "-x" pathToExecutable "--" "-p" port "-f" pathToCommonFood
        
    stopOldServerBy :: PID -> Segment ()
    stopOldServerBy pid = startStopDaemon "--stop" "-o" "-q" "--pid" pid

    switchNginxToNewServer :: FromPort -> ToPort -> Segment ()
    switchNginxToNewServer (From currentPort) (To newPort) = do
        sudo "sed" "-i" replaceProxyPass pathToNginxConfig
        sudo "nginx" "-s" "reload"
      where
        replaceProxyPass = mconcat ["s#", proxyPass <> currentPort, "#", proxyPass <> newPort, "#g"]
        proxyPass        = "proxy_pass http://127.0.0.1:"

    -- Main constants.
    executableName      = "breadu-exe"                       
    rootDirectory       = "/home/dshevchenko/breadu-root"
    pathToCommonFood    = rootDirectory </> "food/common"
    pathToExecutable    = rootDirectory </> executableName
    pathToTmpExecutable = "/tmp" </> executableName
    pathToNginxConfig   = "/etc/nginx/sites-enabled/default"
    defaultPort         = "3000"
    alternativePort     = "3010"
