{-# LANGUAGE DoAndIfThenElse #-}
module Interface.Args (
        Dirs (..),
        SPPOpts(..),
        InPlaceBackup(..),
        SeparateBackup(..),
        processArguments
    ) where

import System.Console.ArgParser
import System.Console.ArgParser.Parser
import Control.Applicative

import System.Directory
import System.Exit

data InPlaceBackup = InPlaceBackup {
    srcAndOut :: String,
    onlyBak :: String
}

data SeparateBackup = SeparateBackup {
    srcAndBak :: String,
    onlyOut :: String
}

data Dirs = InPlace InPlaceBackup | Separate SeparateBackup

data RawDirs = RawDirs {
    rawSrc :: String,
    rawBak :: String,
    rawOut :: String
}

data SPPOpts = Clean {
        dirs :: Dirs
    } |
    Preprocess {
        dirs :: Dirs,
        directiveStart :: String,
        noCleanOnErrors :: Bool
    }

processArguments :: (SPPOpts -> IO ()) -> IO ()
processArguments program = withParseResult optionParser $ \opts -> sanitizeOptions opts >>= program

data Options = Options {
    -- Whether or not this is a cleaning run
    clean :: Bool,
    -- The source directory to preprocess or clean
    rawSrcDir :: String,
    -- Where the output files should go.
    rawOutDir :: Maybe String,
    -- Where the backup files should go.
    rawBakDir :: Maybe String,
    -- The string to start directives with
    rawDirectiveStart :: Maybe String,
    -- Clean up on errors
    rawNoCleanOnErrors :: Bool
}

--TODO add update support

checkDirs :: RawDirs -> IO Dirs
checkDirs (RawDirs src out bak) = do
        csrc <- canonicalizePath src
        cout <- canonicalizePath out
        cbak <- canonicalizePath bak
        fromCanonicalTriple csrc cout cbak
    where
    fromCanonicalTriple :: String -> String -> String -> IO Dirs
    fromCanonicalTriple csrc cout cbak
        | cout == cbak =
            putStrLn "The backup and output directories cannot be the same" >> exitFailure
        | csrc == cout =
            return $ InPlace $ InPlaceBackup {srcAndOut=csrc, onlyBak=cbak}
        | csrc == cbak =
            return $ Separate $ SeparateBackup{srcAndBak=csrc, onlyOut=cout}
        | otherwise =
            putStrLn "The source must be the same as the backup or the output. Otherwise, the backup is redundant" >> exitFailure


sanitizeOptions :: Options -> IO SPPOpts
sanitizeOptions opts
        | clean opts    = case (rawDirectiveStart opts, rawNoCleanOnErrors opts) of
            (Nothing, False) -> Clean <$> extractDirs opts
            (Just _, _) -> errorDie "--directive-start should not be used with --clean"
            (_, True) -> errorDie "--no-clean-on-errors should not be used with --clean"
        | otherwise     = do
                    dirse <- extractDirs opts
                    return $ Preprocess dirse (maybe "" id (rawDirectiveStart opts)) (rawNoCleanOnErrors opts)

extractDirs :: Options -> IO Dirs
extractDirs opts = extractRawDirs opts >>= checkDirs

errorDie :: String -> IO a
errorDie str = putStrLn str >> exitFailure >> return undefined

extractRawDirs :: Options -> IO RawDirs
extractRawDirs opts = fromSrcOutBak (rawSrcDir opts) (rawOutDir opts) (rawBakDir opts)
    where
    fromSrcOutBak src (Just out) (Just bak)
        | out == bak    = errorDie "The backup and output directories cannot be the same"
        | otherwise     = return $ RawDirs {rawSrc=src, rawOut=out, rawBak=bak}
    fromSrcOutBak src Nothing (Just bak)
        = fromSrcOutBak src (Just src) (Just bak)
    fromSrcOutBak src (Just out) Nothing
        = fromSrcOutBak src (Just out) (Just src)
    fromSrcOutBak src Nothing Nothing
        = fromSrcOutBak src (Just src) (Just $ src ++ ".bak")


nothingString :: String
nothingString = "\x01"

optionParser :: ParserSpec Options
optionParser = Options `parsedBy`
        boolFlag "clean" `andBy`
        reqFlag "src" `andMaybeBy`
        optFlag nothingString "out" `andMaybeBy`
        optFlag nothingString "bak" `andMaybeBy`
        optFlag nothingString "directive-start" `andBy`
        boolFlag "no-clean-on-errors"

infixl 1 `andMaybeBy`

andMaybeBy :: ParamSpec spec => ParserSpec (Maybe String -> b) -> spec String -> ParserSpec b
andMaybeBy prev spec = fmap (.isNothing) prev `andBy` spec
    where
    isNothing x
        | x == nothingString    = Nothing
        | otherwise             = Just x
