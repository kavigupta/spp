#!/usr/bin/runhaskell
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Monad
import Control.Applicative hiding ((<|>))

import System.Console.ArgParser.Run
import System.Exit

import FileHandler
import Interface.Args
import CommandGenerator
import Directive.Identifier

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
        = case result of
            Left err -> return $ Left err
            Right res -> res
    where
        result :: Either String (IO (Either String String))
        result = do
            (header, commands, rest) <- parseDirectives path (directiveStart opts) str :: Either String (String, [Action], String)
            return $ fmap (header ++) <$> performAll commands rest
{-
    Perform all the actions in the given list of actions.
    If any of the values are `Left` errors, the entire result is an error.
    Otherwise, the result is considered to be all the other actions chained together
-}
performAll :: (Monad m) => forall a err. [a -> m (Either err a)] -> a -> m (Either err a)
performAll = foldr comp (return . return)
    where
    comp f g x = bind2 (f x) g
    bind2 val f = do
        x <- val
        either (return . Left) f x
