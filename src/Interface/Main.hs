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
    | clean opts = void $ removeBackups (srcDir opts)
    | otherwise  = runPreprocessor opts

{-
Runs the preprocessor on the given options
-}
runPreprocessor :: Options -> IO ()
runPreprocessor opts = do
    bufold <- createBackups $ srcDir opts
    results <- forM (backups bufold) $ preprocess opts
    let failure = concatErrors results
    if isError failure && not (noCleanOnErrors opts) then do
            print failure
            removeBackups $ srcDir opts
            exitFailure
        else
            forM_ (backups bufold) $ setWritable False . originalFile
