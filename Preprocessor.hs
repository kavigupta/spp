#!/usr/bin/runhaskell
module Main (main) where

import System.Directory
import Data.List
import Control.Monad
import Text.Parsec.Token
import Text.Parsec
import Data.Functor.Identity
import Text.RegexPR
import Text.Parsec.Language (haskellDef)

rawposts = "_posts-raw"
posts = "_posts"

quoted = "\"(?<!\\\\)([\\\\\\\\]+)\""

main :: IO ()
main
    = do
        contents <- getDirectoryContents rawposts
        let files = filter (".md" `isSuffixOf`) contents
        forM_ files preprocess

preprocess :: FilePath -> IO ()
preprocess name =
        do
            contents <- readFile inp
            output <- process . lines $ contents
            writeFile out (unlines output)
    where
    inp = rawposts ++ "/" ++ name
    out = posts ++ "/" ++ name


process :: [String] -> IO [String]
process ("preprocess:":xs)
        = performAll commands rest
    where
    (commandlines, rest) = span startsWithTab xs
    commandnames = map tail commandlines
    commands = map (getCommand . parseCommand) commandnames
    startsWithTab str = "\t" `isPrefixOf` str || "    " `isPrefixOf` str
process x = return x

performAll :: [a -> IO a] -> a -> IO a
performAll [] x = return x
performAll (f:fs) x = f x >>= performAll fs

getCommand :: Command -> [String] -> IO [String]
getCommand (Replace regex replacement) = return . lines . subRegexPR regex replacement . unlines
getCommand (CodeDump name ext)
    = return

data Command =
    Replace String String |
    CodeDump String String

type Parser x = ParsecT String String Data.Functor.Identity.Identity x

parseCommand :: String -> Command
parseCommand input
    = case runIdentity $ runParserT command "(unknown)" input "" of
        Left err -> error $ "Invalid command " ++ input
        Right x -> x

command :: Parser Command
command = replace

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
