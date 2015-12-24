#!/usr/bin/runhaskell
import Distribution.Simple

import System.Process
import System.Exit

import Control.Monad

import Text.Regex.Posix((=~))

main = do
    buildCode <- rawSystem "cabal" ["build"]
    when (buildCode /= ExitSuccess) $ die "Build failed"
    hlintRun
    buildTest <- rawSystem "cabal" ["test"]
    when (buildTest /= ExitSuccess) $ die "Build failed"

hlintRun :: IO ()
hlintRun = do
    (_,lintResult1, lintResult2) <- readProcessWithExitCode "hlint" ["."] ""
    let lintResult = lintResult1 ++ lintResult2
    when (lintResult =~ "([0-9]+)\\s+suggestions?")
        $ die ("Hlint suggestions ==>\n" ++ lintResult)

die :: String -> IO ()
die msg = putStrLn msg >> exitFailure
