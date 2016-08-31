{-# OPTIONS -fno-warn-unused-do-bind #-}
module CommandGenerator(
        getCommand, Action, sppHandler,
            PreprocessorResult(..), ToPreprocess(..),
            SPPState(..), mapOverSuccess, mapOverContents
    ) where

import Directive.Parser
import Tools.Shell
import Interface.Errors

import System.Process(readCreateProcess, shell)
import System.FilePath (takeDirectory)
import System.Directory(removeFile, doesFileExist, canonicalizePath)

import Text.Regex
import Control.Monad

import Control.Exception(catch, IOException)
import FileHandler(BackedUpFile, outputFile, sourceFile, sourceIs)

import Data.List(find, delete)
import Text.Parsec
import Tools.Parser


type Preprocessor = ToPreprocess -> IO (Either SPPError ())

data ToPreprocess = ToPreprocess {
          current :: BackedUpFile
        , future :: [BackedUpFile]
        , depChain :: [BackedUpFile]
    }

type Action = SPPState -> IO PreprocessorResult

data SPPState = SPPState {
          cFile :: BackedUpFile
        , fContents :: String
        , dependencyChain :: [BackedUpFile]
        , possibleFiles :: [BackedUpFile]
    } deriving (Show)

data PreprocessorResult = SPPSuccess SPPState |
        SPPFailure SPPError
            deriving (Show)

mapOverSuccess :: (SPPState -> SPPState) -> PreprocessorResult -> PreprocessorResult
mapOverSuccess f (SPPSuccess x) = SPPSuccess . f $ x
mapOverSuccess _ x              = x

mapOverContents :: (String -> String) -> SPPState -> SPPState
mapOverContents f x = x {fContents = f $ fContents x}

sppHandler :: IOExcHandler -> IOException -> IO PreprocessorResult
sppHandler handler = return . SPPFailure . handler

-- Applies the given command in
getCommand :: Preprocessor -> BackedUpFile -> Command -> Action
getCommand preprocessor buf cmd item = inDir (takeDirectory out) wwp
    where
    out = outputFile buf
    path = sourceFile buf
    wwp = withWrittenPath out action
    action = uscmd preprocessor path cmd item
        
-- Gets the action for the given system command
uscmd :: Preprocessor -> FilePath -> Command -> Action
uscmd _ _ (Replace regex replacement) original
        = return . SPPSuccess $ original {fContents=newText}
    where
    originalText = fContents original
    newText = subRegex (mkRegex regex) originalText replacement
uscmd _ path (Exec toExec) original
        = executeAndOutputOriginal path toExec original
uscmd _ path (PassThrough toPass) original
        = ((\x -> SPPSuccess (original{fContents=x})) <$> sh)
            `catch` sppHandler
                (sppError (PassThroughError toPass) path (Just . fContents $ original))
    where
    sh = pass toPass . fContents $ original -- TODO ignoring exit code
uscmd _ path DoWrite original 
        = processParseResult
            (sppError WriteIOError path (Just . fContents $ original))
            writeChunk
            (\u -> (\x -> SPPSuccess $ original {fContents=x}) <$> processOutput u)
            (sppError WriteParseError path (Just . fContents $ original))
            original
uscmd preprocessor path DoInclude original
        = processParseResult
            (sppError IncludeIOError path (Just . fContents $ original))
            includeChunk
            (processInclude preprocessor original)
            (sppError IncludeParseError path (Just . fContents $ original))
            original

pass :: String -> String -> IO String
pass toPass = readCreateProcess (shell toPass)

withWrittenPath :: FilePath -> IO a -> IO a
withWrittenPath path f = do
    exists <- doesFileExist ".spp-current-file"
    when exists $ removeFile ".spp-current-file"
    writeFile ".spp-current-file" path 
    result <- f
    exists' <- doesFileExist ".spp-current-file"
    when exists' $ removeFile ".spp-current-file"
    return result 

-- Executes the given command and outputs the original text
--  This should not throw errors, since it catches all of them in the either handler
executeAndOutputOriginal :: FilePath -> String -> Action
executeAndOutputOriginal path toExec original = do
    result <- fmap (\x -> SPPSuccess $ original {fContents = x}) (pass toExec $ fContents original)
        `catch` sppHandler (sppError (ExecError toExec) path (Just . fContents $ original))
    return $ case result of
        (SPPFailure x) -> SPPFailure x
        _ -> SPPSuccess original

-- Processes the parse result.
--  This should not throw errors, since it catches all of them and wraps them up in the internal Either.
processParseResult :: IOExcHandler -> Parser a -> (([String], [a]) -> IO PreprocessorResult) -> ParseHandler -> Action
processParseResult iohandler parser f parsehandler input
    = case doParse (intersperse parser) (fContents input) of
        (Left err) -> return . SPPFailure . parsehandler $ err
        (Right x) -> f x `catch` sppHandler iohandler

-- dumps the given filepaths and strings to a file, then returns the original non-write chuncks concatenated
processOutput :: ([String], [(FilePath, String)]) -> IO String
processOutput (newText, toWrite) = mapM_ (uncurry writeFile) toWrite >> return (concat newText)

-- Reads the files for each path to include. Then intercalates these with the originals
processInclude :: Preprocessor -> SPPState -> ([String], [FilePath]) -> IO PreprocessorResult
processInclude preprocesor state (strs, paths) =
    do
        dependencyInsurance <- ensureAllDependencies preprocesor state paths
        case dependencyInsurance of
            SPPSuccess state' -> do
                readFiles <- forM paths readFile
                let paddedFiles = readFiles ++ repeat ""
                let pad = zipWith (++) strs paddedFiles
                return . SPPSuccess $ state' {fContents = concat pad}
            SPPFailure err -> return $ SPPFailure err

ensureAllDependencies :: Preprocessor -> SPPState -> [FilePath] -> IO PreprocessorResult
ensureAllDependencies _ state [] = return $ SPPSuccess state
ensureAllDependencies pre state (x:xs) = do
    path' <- canonicalizePath x
    c <- ensureDependencies pre state path'
    case c of
        SPPSuccess state' -> ensureAllDependencies pre state' xs
        SPPFailure err -> return $ SPPFailure err


ensureDependencies :: Preprocessor -> SPPState -> FilePath -> IO PreprocessorResult
ensureDependencies preprocess state@SPPState {dependencyChain=stack, possibleFiles=futu} path
        | any (`sourceIs` path) stack
            = return . SPPFailure $ SPPError (CircularDependencyError (map sourceFile stack ++ [path])) path Nothing Nothing
        | otherwise
            = case find (`sourceIs` path) futu of
                Nothing -> return . SPPSuccess $ state
                (Just x) -> 
                        let future' = delete x futu
                            depChain' = stack ++ [cFile state]
                        in do
                            resultpp <- preprocess ToPreprocess {current=x, future=future', depChain=depChain'}
                            case resultpp of
                                Right () ->
                                    return . SPPSuccess $ state {dependencyChain = depChain', possibleFiles=future'}
                                Left err -> return $ SPPFailure err
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
    string "write"
    skipMany nonNewlineSpace
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
    string "include"
    skipMany nonNewlineSpace
    haskellString
