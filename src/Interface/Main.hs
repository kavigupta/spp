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
doProcessing (Clean _ directories) = runClean directories
doProcessing opts@(Preprocess {}) = runPreprocessor opts

{-
Runs the preprocessor on the given options
-}
runPreprocessor :: SPPOpts -> IO ()
runPreprocessor sppopts = do
    maybeBak <- createBackups $ dirs sppopts
    bks <- onErrorExit maybeBak errorReporter
    results <- forM bks $ preprocess sppopts
    forM_ bks $ setWritable False . outputFile
    let perhapsfail = actualError $ concatErrors results
    onErrorExit perhapsfail $ \failure -> do
            errorReporter failure
            unless (noCleanOnErrors sppopts) $ do
                backfail <- removeBackups UponFailure (dirs sppopts)
                onErrorExit backfail errorReporter

runClean :: Dirs -> IO ()
runClean root = do
    backfail <- removeBackups UponRequest root
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
