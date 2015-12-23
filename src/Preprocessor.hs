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
import ShellHandler

import Control.Exception

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

{-
An IO Action for either processing a file or producing an error without doing anything
-}
unsafePreprocess :: BackedUpFile -> IO (Either String ())
unsafePreprocess buFile =
        do
            contents <- readFile $ backupFile buFile
            output <- process (originalFile buFile) . lines $ contents
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
            Right f -> f (unlines rest)
    where
    (commandlines, rest) = span startsWithTab xs
        where
        startsWithTab str = "\t" `isPrefixOf` str || "    " `isPrefixOf` str
    commandnames :: [String]
    commandnames = map tail commandlines -- TODO FIX
    commands :: [Either String Action]
    commands = map (toCommand path) commandnames
process _ x = return . Right . unlines $ x

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
