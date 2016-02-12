#!/usr/bin/runhaskell
import Distribution.Simple

import System.Process
import System.Exit
import System.Directory

import Control.Monad

import Text.Regex.Posix((=~))

main = do
    configureCode <- rawSystem "cabal" ["configure", "--enable-tests"]
    buildCode <- rawSystem "cabal" ["build"]
    when (buildCode /= ExitSuccess) $ die "Build failed"
    hlintRun
    copyFile "dist/build/spp/spp" "spp"
    exists <- doesFileExist "log"
    when exists $ removeFile "log"
    buildTest <- rawSystem "cabal" ["test"]
    when (buildTest /= ExitSuccess) $ die "Tests failed"

hlintRun :: IO ()
hlintRun = do
    (_,lintResult1, lintResult2) <- readProcessWithExitCode "hlint" ["src"] ""
    let lintResult = lintResult1 ++ lintResult2
    when (lintResult =~ "([0-9]+)\\s+suggestions?")
        $ die ("Hlint suggestions ==>\n" ++ lintResult)

die :: String -> IO ()
die msg = putStrLn msg >> exitFailure
