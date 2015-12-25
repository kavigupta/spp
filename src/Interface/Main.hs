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
    maybeBak <- createBackups $ srcDir opts
    bufold <- onErrorExit maybeBak print
    results <- forM (backups bufold) $ preprocess opts
    let failure = concatErrors results
    if isError failure && not (noCleanOnErrors opts) then do
            print failure
            removeBackups $ srcDir opts
            exitFailure
        else
            forM_ (backups bufold) $ setWritable False . originalFile

onErrorExit :: Either SPPError a -> (SPPError -> IO b) -> IO a
onErrorExit (Left err) handler = handler err >> exitFailure
onErrorExit (Right x) _        = return x
