module CommandGenerator(
        toCommand, Action
    ) where

import Directive.Parser
import Tools.Files
import Tools.Shell

import System.Process(readProcess, system)
import System.FilePath

import Text.Regex
import Control.Applicative hiding ((<|>), many)
import Control.Monad

import Control.Exception(catch)

import Text.Parsec
import Tools.Parser

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
        = executeAndOutputOriginal toExec original
uscmd path (PassThrough toPass) original
        = fmap Right (readProcess toPass [path] original) `catch` eitherHandler
uscmd _ DoWrite original
        = processParseResult writeChunk processOutput original
uscmd _ DoInclude original
        = processParseResult includeChunk processInclude original

-- Executes the given command and outputs the original text
--  This should not throw errors, since it catches all of them in the either handler
executeAndOutputOriginal :: String -> Action
executeAndOutputOriginal toExec original = do
    result <- fmap Right (system toExec) `catch` eitherHandler
    return $ result >>= const (Right original)

-- Processes the parse result.
--  This should not throw errors, since it catches all of them and wraps them up in the internal Either.
processParseResult :: Parser a -> (([String], [a]) -> IO String) -> Action
processParseResult parser f input
    = case doParse (intersperse parser) input of
        (Left err) -> return $ Left . show $ err
        (Right x) -> (Right <$> f x) `catch` eitherHandler

-- dumps the given filepaths and strings to a file, then returns the original non-write chuncks concatenated
processOutput :: ([String], [(FilePath, String)]) -> IO String
processOutput (newText, toWrite) = mapM_ (uncurry writeFile) toWrite >> return (concat newText)

-- Reads the files for each path to include. Then intercalates these with the originals
processInclude :: ([String], [FilePath]) -> IO String
processInclude (strs, paths) =
    do
        readFiles <- forM paths readFile
        let paddedFiles = readFiles ++ repeat ""
        let pad = zipWith (++) strs paddedFiles
        return $ concat pad

{-
Applies the given parser multiple times, returning a list of all the interspersed strings as well
-}
intersperse :: Parser a -> Parser ([String], [a])
intersperse parser = do
    regular <- manyTill anyChar $ eof <|> (try . void . lookAhead $ parser)
    writeResult <- (eof >> return Nothing) <|> liftM Just parser
    case writeResult of
        Nothing -> return ([regular], [])
        Just result -> do
            (nexts, nextw) <- intersperse parser
            return (regular:nexts, result:nextw)

{- Parses a write directive of the form
    write: "path" <- ~~~
    to write blah blah blah
    ~~~
-}
writeChunk :: Parser (FilePath, String)
writeChunk = do
    string "write:"
    spaces
    path <- haskellString
    spacedString "<-"
    string "~~~"
    toBeWritten <- manyTill anyChar $ try (string "~~~")
    return (path, toBeWritten)

{- Parses an include directive of the form
    "include:"
-}
includeChunk :: Parser FilePath
includeChunk = do
    string "include:"
    spaces
    haskellString
