#!/usr/bin/runhaskell
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Monad
import Control.Applicative hiding ((<|>))
import Text.Regex.Posix

import System.Console.ArgParser.Run
import System.Exit

import FileHandler
import ArgumentProcessor
import CommandGenerator

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
    case failure of
        (Just err) -> do
            putStrLn err
            removeBackups $ srcDir opts
            exitFailure
        Nothing -> forM_ (backups bufold) $ setWritable False . originalFile

{-
Collects all `Left` errors from the given list of Eithers
-}
concatErrors :: [Either a b] -> Maybe a
concatErrors [] = Nothing
concatErrors (x:xs) = either Just (const $ concatErrors xs) x

{-
An IO Action for either processing a file or producing an error without doing anything
-}
preprocess :: Options -> BackedUpFile -> IO (Either String ())
preprocess opts buFile =
        do
            -- Ignore the possibility of error at this line.
            contents <- readFile $ backupFile buFile
            -- There should be no error at this line given that process should throw no error
            output <- process opts (originalFile buFile) contents
            case output of
                Left err -> return $ Left err
                Right outputValue -> Right <$> writeFile (originalFile buFile) outputValue

{-
Creates an IO instance for preprocessing a series of lines.
-}
process :: Options -> FilePath -> String -> IO (Either String String)
process opts path str
        = case performAll commands of
            Left err -> return $ Left err
            Right f -> f rest
    where
    (commandnames, rest) = collectDirectives (directiveStart opts) str
    commands :: [Either String Action]
    commands = map (toCommand path) commandnames

collectDirectives :: String -> String -> ([String], String)
collectDirectives start str
        = case str =~ regex of
            [[_, values, rest]] -> (getDirectives values, rest)
            _ -> ([], str)
    where
    getDirectives = map (dropWhile (`elem` "\t ")) . lines
    regex = start ++ "preprocess:\\r?\\n(\\s+" ++ start ++ ".+\\r?\\n)+"
{-
    Perform all the actions in the given list of actions.
    If any of the values are `Left` errors, the entire result is an error.
    Otherwise, the result is considered to be all the other actions chained together
-}
performAll :: forall a err. [Either err (a -> IO (Either err a))] -> Either err (a -> IO (Either err a))
performAll = foldr comp (Right $ return . Right)
    where
    comp :: Either err (a -> IO (Either err a)) -> Either err (a -> IO (Either err a)) -> Either err (a -> IO (Either err a))
    comp = either (const . Left) feed
    feed :: (a -> IO (Either err a)) -> Either err (a -> IO (Either err a)) -> Either err (a -> IO (Either err a))
    feed _ (Left err) = Left err
    feed f (Right g) = Right (f >=> either (return . Left) g)
