#!/usr/bin/runhaskell
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.List
import Control.Monad
import Control.Applicative hiding ((<|>))
import System.Console.ArgParser.Run
import FileHandler
import ArgumentProcessor
import CommandGenerator

import Control.Exception

main :: IO ()
main = withParseResult optionParser doProcessing

doProcessing :: Options -> IO ()
doProcessing opts
    | clean opts = removeBackups (srcDir opts) >> return ()
    | otherwise  = runPreprocessor opts

{-
Runs the preprocessor on the given options
-}
runPreprocessor :: Options -> IO ()
runPreprocessor opts = do
    bufold <- createBackups $ srcDir opts
    results <- forM (backups bufold) preprocess
    let failure = concatErrors results
    case failure of
        (Just err) -> do
            putStrLn "FAILURE!!!"
            putStrLn err
            removeBackups (srcDir opts)
            return ()
        Nothing -> forM_ (backups bufold) $ setWritable False . originalFile

{-
Collects all `Left` errors from the given list of Eithers
-}
concatErrors :: [Either a b] -> Maybe a
concatErrors [] = Nothing
concatErrors (Right _:xs) = concatErrors xs
concatErrors (Left err:_) = Just err

preprocess :: BackedUpFile -> IO (Either String ())
preprocess out = unsafePreprocess out `catch` eitherHandler

eitherHandler :: IOException -> IO (Either String ())
eitherHandler err = return . Left $ "An error occured in preprocessing: " ++ show err

{-
An IO Action for either processing a file or producing an error without doing anything
-}
unsafePreprocess :: BackedUpFile -> IO (Either String ())
unsafePreprocess buFile =
        do
            contents <- readFile $ backupFile buFile
            output <- (process (originalFile buFile) . lines $ contents)
            case output of
                Left err -> return $ Left err
                Right outputValue -> Right <$> writeFile (originalFile buFile) outputValue

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
