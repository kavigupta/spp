#!/usr/bin/runhaskell
module Main (main) where

import System.Directory
import Data.List
import Control.Monad
import Control.Applicative hiding ((<|>))
import System.Console.ArgParser.Run
import FileHandler
import ArgumentProcessor
import DirectiveParser

import Control.Exception

main :: IO ()
main = withParseResult optionParser doProcessing

doProcessing :: Options -> IO ()
doProcessing opts
    | clean opts = runClean opts
    | otherwise  = runPreprocessor opts

runPreprocessor :: Options -> IO ()
runPreprocessor opts = do
    files <- allFiles $ srcDir opts
    createBackups files
    results <- forM files preprocess
    let failure = concatenateEither results
    case failure of
        (Just err) -> putStrLn "FAILURE!!!" >> putStrLn err >> removeFiles files
        Nothing -> forM_ files $ setWritable False

concatenateEither :: [Either a b] -> Maybe a
concatenateEither [] = Nothing
concatenateEither ((Right _):xs) = concatenateEither xs
concatenateEither ((Left err):_) = Just err

runClean :: Options -> IO ()
runClean opts = do
    contents <- allFiles $ srcDir opts
    files <- removeGenerated contents
    removeFiles files

removeFiles :: [FilePath] -> IO ()
removeFiles files = do
    forM_ files (setWritable True) `catch` handleExists
    forM_ files $ liftM2 (>>) (putStrLn . ("Removing: "++)) removeIfExists
    forM_ files $ \path -> renameFile (makeBackup path) path

preprocess :: FilePath -> IO (Either String ())
preprocess out =
        do
            contents <- readFile $ makeBackup out
            output <- process . lines $ contents
            case output of
                Left err -> return $ Left err
                Right outputValue -> Right <$> writeFile out outputValue

process :: [String] -> IO (Either String String)
process ("preprocess:":xs) =
        case performAll commands of
            Left err -> return $ Left err
            Right f -> Right <$> f (unlines rest)
    where
    (commandlines, rest) = span startsWithTab xs
        where
        startsWithTab str = "\t" `isPrefixOf` str || "    " `isPrefixOf` str
    commandnames = map tail commandlines
    commands :: [Either String (String -> IO String)]
    commands = map toCommand commandnames
process x = return . Right . unlines $ x

performAll :: [Either err (a -> IO a)] -> Either err (a -> IO a)
performAll [] = Right return
performAll ((Left err):_)     = Left err
performAll ((Right f):fs) = fmap (\rfunc -> (\x -> f x >>= rfunc)) $ performAll fs
