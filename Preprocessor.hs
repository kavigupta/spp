#!/usr/bin/runhaskell
module Main (main) where

import System.Directory
import System.Exit
import Data.List
import Control.Monad
import Control.Applicative hiding ((<|>))
import Text.Parsec.Token
import Text.Parsec
import Data.Functor.Identity
import Text.Regex
import Text.Parsec.Language (haskellDef)
import System.Environment
import System.Console.ArgParser.Run
import FileHandler
import ArgumentProcessor

import Prelude hiding (catch)
import Control.Exception
import System.IO.Error hiding (catch)

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
    results <- forM files $ preprocess opts
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
    putStrLn $ "Removing : " ++ show files
    forM_ files removeIfExists
    forM_ files $ \path -> renameFile (makeBackup path) path

preprocess :: Options -> FilePath -> IO (Either String ())
preprocess opts out =
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
    commands = map (\x -> getCommand <$> parseCommand x) commandnames
process x = return . Right . unlines $ x

performAll :: [Either err (a -> IO a)] -> Either err (a -> IO a)
performAll [] = Right return
performAll ((Left err):_)     = Left err
performAll ((Right f):fs) = fmap (\rfunc -> (\x -> f x >>= rfunc)) $ performAll fs

getCommand :: Command -> String -> IO String
getCommand (Replace regex replacement)= return . replacer
    where
    replacer x = subRegex (mkRegex regex) x replacement
getCommand (CodeDump name ext) = return

data Command =
    Replace String String |
    CodeDump String String

type Parser x = ParsecT String String Data.Functor.Identity.Identity x

parseCommand :: String -> Either String Command
parseCommand input
    = case runIdentity $ runParserT command "(unknown)" "" input of
        Left err -> Left $ "Invalid command " ++ input ++ "\n" ++ show err
        Right x -> Right x

command :: Parser Command
command = do
    spaces
    replace <|> codeDump

haskellString :: Parser String
haskellString = stringLiteral (makeTokenParser haskellDef)

replace :: Parser Command
replace = do
    string "replace:"
    spaces
    regex <- haskellString
    spaces
    string "->"
    spaces
    replacement <- haskellString
    return $ Replace regex replacement

codeDump :: Parser Command
codeDump = do
    string "code-dump:"
    spaces
    string "language-name"
    spaces
    string "="
    spaces
    name <- haskellString
    spaces
    string "language-ext"
    spaces
    string "="
    spaces
    ext <- haskellString
    return $ CodeDump name ext
