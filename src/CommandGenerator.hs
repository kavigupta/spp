module CommandGenerator(
        toCommand
    ) where

import DirectiveParser
import ShellHandler

import System.Process(readProcess, system)
import System.FilePath

import Text.Regex
import Control.Applicative hiding ((<|>), many)
import Control.Monad

import Text.Parsec

toCommand :: FilePath -> String -> Either String (String -> IO String)
toCommand path str = getCommand path <$> parseCommand str

getCommand :: FilePath -> Command -> String -> IO String
getCommand path cmd original = inDir (takeDirectory path) $ getUnShiftedCommand path cmd original

getUnShiftedCommand :: FilePath -> Command -> String -> IO String
getUnShiftedCommand _ (Replace regex replacement) original
        = return replaced
    where replaced = subRegex (mkRegex regex) original replacement
getUnShiftedCommand _ (Exec toExec) original
        = system toExec >> return original
getUnShiftedCommand path (PassThrough toPass) original
        = readProcess toPass [path] original
getUnShiftedCommand _ DoWrite original
        = processParseResult writeChunk processOutput original
getUnShiftedCommand _ DoInclude original
        = processParseResult includeChunk processInclude original

processParseResult :: Parser a -> (([String], [a]) -> IO String) -> String -> IO String
processParseResult parser f input
    = case doParse (intersperse parser) input of
        Left err -> putStrLn (show err) >> return input
        Right x -> f x
--getUnShiftedCommand _ DoWrite original = undefined
--getUnShiftedCommand _ DoInclude original = undefined

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
