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

main :: IO ()
main = withParseResult optionParser doProcessing

doProcessing :: Options -> IO ()
doProcessing opts
    | clean opts = runClean opts
    | otherwise  = runPreprocessor opts

runPreprocessor :: Options -> IO ()
runPreprocessor opts = do
    contents <-allFiles $ srcDir opts
    createBackups contents
    forM_ contents $ preprocess opts
    forM_ contents $ setWritable False

runClean :: Options -> IO ()
runClean opts = do
    contents <- allFiles $ srcDir opts
    let files = filter (not . isSuffixOf ".bak") contents
    bakExists <- forM (map (++ ".bak") files) doesFileExist
    let fWithBackup = zip bakExists files
    let existingBak = find (not . fst) fWithBackup
    case existingBak of
        (Just (_, path)) -> putStrLn ("No backup exists for file " ++ path) >> exitFailure
        Nothing -> return ()
    forM_ files (setWritable True)
    putStrLn $ "Removing : " ++ show files
    forM_ files removeFile
    forM_ files $ \path -> renameFile (path ++ ".bak") path

preprocess :: Options -> FilePath -> IO ()
preprocess opts out =
        do
            contents <- readFile inp
            output <- process . lines $ contents
            writeFile out (output)
    where
    inp = out ++ ".bak"

process :: [String] -> IO String
process ("preprocess:":xs)
        = performAll commands $ unlines rest
    where
    (commandlines, rest) = span startsWithTab xs
        where
        startsWithTab str = "\t" `isPrefixOf` str || "    " `isPrefixOf` str
    commandnames = map tail commandlines
    commands = map (getCommand . parseCommand) commandnames
process x = return $ unlines x

performAll :: [a -> IO a] -> a -> IO a
performAll [] x = return x
performAll (f:fs) x = f x >>= performAll fs

getCommand :: Command -> String -> IO String
getCommand (Replace regex replacement)= return . replacer
    where
    replacer x = subRegex (mkRegex regex) x replacement
getCommand (CodeDump name ext) = return

data Command =
    Replace String String |
    CodeDump String String

type Parser x = ParsecT String String Data.Functor.Identity.Identity x

parseCommand :: String -> Command
parseCommand input
    = case runIdentity $ runParserT command "(unknown)" "" input of
        Left err -> error $ "Invalid command " ++ input ++ "\n" ++ show err
        Right x -> x

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
