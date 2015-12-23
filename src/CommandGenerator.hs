module CommandGenerator(
        toCommand, Action
    ) where

import DirectiveParser
import ShellHandler

import System.Process(readProcess, system)
import System.FilePath

import Text.Regex
import Control.Applicative hiding ((<|>), many)
import Control.Monad

import Control.Exception(catch)

import Text.Parsec

type Errored = Either String String

type Action = String -> IO Errored

-- Converts a directive into a command, possibly producing an error.
toCommand :: FilePath -> String -> Either String Action
toCommand path str = getCommand path <$> parseCommand str

-- Applies the given command in
getCommand :: FilePath -> Command -> Action
getCommand path cmd = inDir (takeDirectory path) . uscmd path cmd

-- Gets the action for the given system command
uscmd :: FilePath -> Command -> Action
uscmd _ (Replace regex replacement) original
        = return . Right $ subRegex (mkRegex regex) original replacement
uscmd _ (Exec toExec) original
        = do
            result <- fmap Right (system toExec) `catch` eitherHandler
            case result of
                Left err -> return (Left err)
                Right _ -> return (Right original)
uscmd path (PassThrough toPass) original
        = fmap Right (readProcess toPass [path] original) `catch` eitherHandler
uscmd _ DoWrite original
        = processParseResult writeChunk processOutput original `catch` eitherHandler
uscmd _ DoInclude original
        = processParseResult includeChunk processInclude original `catch` eitherHandler

processParseResult :: Parser a -> (([String], [a]) -> IO String) -> Action
processParseResult parser f input
    = case doParse (intersperse parser) input of
        (Left err) -> return $ Left . show $ err
        (Right x) -> Right <$> f x

processOutput :: ([String], [(FilePath, String)]) -> IO String
processOutput (newText, toWrite) = sequence (map (uncurry writeFile) toWrite) >> return (concat newText)

processInclude :: ([String], [FilePath]) -> IO String
processInclude (strs, paths) =
    do
        readFiles <- forM paths readFile
        let paddedFiles = readFiles ++ (repeat "")
        let pad = zipWith (++) strs paddedFiles
        return $ concat pad

{-
Applies the given parser multiple times, returning a list of all the interspersed strings as well
-}
intersperse :: Parser a -> Parser ([String], [a])
intersperse parser = do
    regular <- manyTill anyChar $ eof <|> (try $ lookAhead parser >> return ())
    writeResult <- (eof >> return Nothing) <|> (parser >>= (return . Just))
    case writeResult of
        Nothing -> return ([regular], [])
        Just result -> do
            (nexts, nextw) <- intersperse parser
            return (regular:nexts, result:nextw)

writeChunk :: Parser (FilePath, String)
writeChunk = do
    string "write:"
    spaces
    path <- haskellString
    spacedString "<-"
    string "~~~"
    toBeWritten <- manyTill anyChar $ try (string "~~~")
    return (path, toBeWritten)

includeChunk :: Parser FilePath
includeChunk = do
    string "include:"
    spaces
    haskellString
