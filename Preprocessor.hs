#!/usr/bin/runhaskell
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import System.Directory
import Data.List
import Control.Monad
import Control.Applicative hiding ((<|>))
import System.Console.ArgParser.Run
import FileHandler
import ArgumentProcessor
import DirectiveParser
import System.Exit

import Control.Exception

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
    files <- allFiles $ srcDir opts
    when (any isBackup files) $ putStrLn "Backups exist. Perhaps you forgot to run spp --clean" >> exitFailure
    createBackups files
    results <- forM files preprocess
    let failure = concatErrors results
    case failure of
        (Just err) -> putStrLn "FAILURE!!!" >> putStrLn err >> removeFiles files
        Nothing -> forM_ files $ setWritable False

{-
Collects all `Left` errors from the given list of Eithers
-}
concatErrors :: [Either a b] -> Maybe a
concatErrors [] = Nothing
concatErrors (Right _:xs) = concatErrors xs
concatErrors (Left err:_) = Just err

{-
Cleans the given location by removing all its files
-}
runClean :: Options -> IO ()
runClean opts = do
    contents <- allFiles $ srcDir opts
    files <- removeGenerated contents
    removeFiles files

{-
Removes the given files and renames the backups to the original path names
-}
removeFiles :: [FilePath] -> IO ()
removeFiles files = do
    forM_ files (setWritable True) `catch` handleExists
    forM_ files $ liftM2 (>>) (putStrLn . ("Removing: "++)) removeIfExists
    forM_ files $ \path -> renameFile (makeBackup path) path

preprocess :: FilePath -> IO (Either String ())
preprocess out = unsafePreprocess out `catch` (\err -> return . Left $ "An error occured in preprocessing: " ++ show (err :: IOException))

{-
An IO Action for either processing a file or producing an error without doing anything
-}
unsafePreprocess :: FilePath -> IO (Either String ())
unsafePreprocess out =
        do
            contents <- readFile $ makeBackup out
            output <- (process out . lines $ contents)
            case output of
                Left err -> return $ Left err
                Right outputValue -> Right <$> writeFile out outputValue

{-
Creates an IO instance for preprocessing a series of lines.
-}
process :: FilePath -> [String] -> IO (Either String String)
process path ("preprocess:":xs) =
        case performAll commands of
            Left err -> return $ Left err
            Right f -> Right <$> f (unlines rest)
    where
    (commandlines, rest) = span startsWithTab xs
        where
        startsWithTab str = "\t" `isPrefixOf` str || "    " `isPrefixOf` str
    commandnames = map tail commandlines
    commands :: [Either String (String -> IO String)]
    commands = map (toCommand path) commandnames
process _ x = return . Right . unlines $ x

{-
    Perform all the actions in the given list of actions.
    If any of the values are `Left` errors, the entire result is an error.
    Otherwise, the result is considered to be all the other actions chained together
-}
performAll :: forall a err. [Either err (a -> IO a)] -> Either err (a -> IO a)
performAll fs = foldr (>=>) return <$> sequence fs
