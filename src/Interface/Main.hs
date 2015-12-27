module Main (
        main
    ) where

import System.Console.ArgParser.Run

import System.Exit

import Preprocessor
import Interface.Args
import FileHandler
import Interface.Errors
import Tools.String

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
    forM_ bks $ setWritable False . originalFile
    let perhapsfail = actualError $ concatErrors results
    onErrorExit perhapsfail $ \failure -> do
            errorReporter failure
            unless (noCleanOnErrors opts) $ do
                backfail <- removeBackups UponFailure $ srcDir opts
                onErrorExit backfail errorReporter

runClean :: Options -> IO ()
runClean opts = do
    backfail <- removeBackups UponRequest (srcDir opts)
    onErrorExit backfail errorReporter

onErrorExit :: Either SPPError a -> (SPPError -> IO b) -> IO a
onErrorExit (Left err) handler = handler err >> exitFailure
onErrorExit (Right x) _        = return x

errorReporter :: SPPError -> IO ()
errorReporter err = do
        width <- terminalWidth
        let val = indentHangs width (show err)
        putStrLn val


terminalWidth :: IO Int
terminalWidth = return 80
