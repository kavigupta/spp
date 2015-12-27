{-# OPTIONS -fno-warn-unused-do-bind #-}
module CommandGenerator(
        getCommand, Action
    ) where

import Directive.Parser
import Tools.Files
import Tools.Shell
import Interface.Errors

import System.Process(readProcess, system)
import System.FilePath

import Text.Regex
import Control.Applicative hiding ((<|>), many)
import Control.Monad

import Control.Exception(catch)

import Text.Parsec
import Tools.Parser

type Errored = Either SPPError String

type Action = String -> IO Errored

-- Applies the given command in
getCommand :: FilePath -> Command -> Action
getCommand path cmd = inDir (takeDirectory path) . uscmd path cmd

-- Gets the action for the given system command
uscmd :: FilePath -> Command -> Action
uscmd _ (Replace regex replacement) original
        = return . Right $ subRegex (mkRegex regex) original replacement
uscmd path (Exec toExec) original
        = executeAndOutputOriginal path toExec original
uscmd path (PassThrough toPass) original
        = fmap Right (readProcess toPass [path] original)
            `catch` eitherHandler (sppError (PassThroughError toPass) path (Just original))
uscmd path DoWrite original
        = processParseResult
            (sppError WriteIOError path (Just original))
            writeChunk
            processOutput
            (sppError WriteParseError path (Just original))
            original
uscmd path DoInclude original
        = processParseResult
            (sppError IncludeIOError path (Just original))
            includeChunk
            processInclude
            (sppError IncludeParseError path (Just original))
            original

-- Executes the given command and outputs the original text
--  This should not throw errors, since it catches all of them in the either handler
executeAndOutputOriginal :: FilePath -> String -> Action
executeAndOutputOriginal path toExec original = do
    result <- fmap Right (system toExec)
        `catch` eitherHandler (sppError (ExecError toExec) path (Just original))
    return $ result >>= const (Right original)

-- Processes the parse result.
--  This should not throw errors, since it catches all of them and wraps them up in the internal Either.
processParseResult :: IOExcHandler -> Parser a -> (([String], [a]) -> IO String) -> ParseHandler -> Action
processParseResult iohandler parser f parsehandler input
    = case doParse (intersperse parser) input of
        (Left err) -> return . Left . parsehandler $ err
        (Right x) -> (Right <$> f x) `catch` eitherHandler iohandler

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
