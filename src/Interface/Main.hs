module Main (
        main
    ) where

import System.Exit

import Preprocessor
import Interface.Args
import FileHandler
import Interface.Errors
import Tools.String

import Control.Monad

main :: IO ()
main = processArguments doProcessing

doProcessing :: SPPOpts -> IO ()
doProcessing opts@(Clean _ directories) = runClean opts directories
doProcessing opts@(Preprocess {}) = runPreprocessor opts

{-
Runs the preprocessor on the given options
-}
runPreprocessor :: SPPOpts -> IO ()
runPreprocessor sppopts = do
    maybeBak <- createBackups $ dirs sppopts
    bks <- onErrorExit maybeBak (errorReporter sppopts)
    results <- preprocessAll sppopts bks
    forM_ bks $ setWritable False . outputFile
    let perhapsfail = actualError $ concatErrors results
    onErrorExit perhapsfail $ failureHandling sppopts

failureHandling :: SPPOpts -> SPPError -> IO ()
failureHandling sppopts failure = do
    errorReporter sppopts failure
    unless (noCleanOnErrors sppopts) $ do
        backfail <- removeBackups UponFailure (dirs sppopts)
        onErrorExit backfail (errorReporter sppopts)


runClean :: SPPOpts -> Dirs -> IO ()
runClean opts root = do
    backfail <- removeBackups UponRequest root
    onErrorExit backfail (errorReporter opts)

onErrorExit :: Either SPPError a -> (SPPError -> IO b) -> IO a
onErrorExit (Left err) handler = handler err >> exitFailure
onErrorExit (Right x) _        = return x

errorReporter :: SPPOpts -> SPPError -> IO ()
errorReporter opts err = do
        width <- terminalWidth
        let val = indentHangs width (show err)
        output opts Info val


terminalWidth :: IO Int
terminalWidth = return 80
