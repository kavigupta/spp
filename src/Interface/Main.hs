module Main (
        main
    ) where

import System.Console.ArgParser.Run

import System.Exit

import Preprocessor
import Interface.Args
import FileHandler
import Interface.Errors

import Control.Monad

main :: IO ()
main = withParseResult optionParser doProcessing

doProcessing :: Options -> IO ()
doProcessing opts
    | clean opts = runClean opts
    | otherwise  = runPreprocessor opts

{-
Runs the preprocessor on the given options
-}
runPreprocessor :: Options -> IO ()
runPreprocessor opts = do
    maybeBak <- createBackups $ srcDir opts
    bks <- onErrorExit maybeBak errorReporter
    results <- forM bks $ preprocess opts
    let failure = concatErrors results
    when (isError failure && not (noCleanOnErrors opts)) $ do
            errorReporter failure
            backfail <- removeBackups UponFailure $ srcDir opts
            onErrorExit backfail errorReporter
            exitFailure
    forM_ bks $ setWritable False . originalFile

runClean :: Options -> IO ()
runClean opts = do
    backfail <- removeBackups UponRequest (srcDir opts)
    onErrorExit backfail errorReporter

onErrorExit :: Either SPPError a -> (SPPError -> IO b) -> IO a
onErrorExit (Left err) handler = handler err >> exitFailure
onErrorExit (Right x) _        = return x

errorReporter :: SPPError -> IO ()
errorReporter = print
