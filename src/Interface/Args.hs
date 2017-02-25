{-# LANGUAGE DoAndIfThenElse #-}
module Interface.Args (
        Dirs (..),
        SourceLocation(..),
        SPPOpts(..),
        output,
        PrintLevel(..),
        processArguments
    ) where

import System.Console.ArgParser
import System.Console.ArgParser.Parser

import System.Directory
import System.Exit

import Control.Monad
import Data.Maybe

data SourceLocation = AtBak | AtOut deriving Show

data Dirs = Dirs {
    bakOf :: FilePath,
    outOf :: FilePath,
    srcLoc :: SourceLocation
} deriving Show

data RawDirs = RawDirs {
    rawSrc :: String,
    rawBak :: String,
    rawOut :: String
}

data SPPOpts = Clean {
        printLevel :: PrintLevel,
        dirs :: Dirs
    } |
    Preprocess {
        printLevel :: PrintLevel,
        dirs :: Dirs,
        directiveStart :: String,
        noCleanOnErrors :: Bool
    }

processArguments :: (SPPOpts -> IO ()) -> IO ()
processArguments program = withParseResult optionParser $ sanitizeOptions >=> program

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
    rawNoCleanOnErrors :: Bool,
    -- Whether or not to print debug statements
    isDebug :: Bool,
    -- Whether or not to print verbose statements
    isVerbose :: Bool
}

--TODO add update support

checkDirs :: PrintLevel -> RawDirs -> IO Dirs
checkDirs level (RawDirs {rawSrc=src,rawOut=out,rawBak=bak}) = do
        csrc <- cleanCanon src
        cout <- cleanCanon out
        cbak <- cleanCanon bak
        u <- fromCanonicalTriple csrc cout cbak
        filtOutput level Verbose $ "You selected the directories " ++ show u ++ "\n"
        return u
    where
    fromCanonicalTriple :: String -> String -> String -> IO Dirs
    fromCanonicalTriple csrc cout cbak
        | cout == cbak =
            filtOutput level Info "The backup and output directories cannot be the same\n" >> exitFailure
        | csrc == cout =
            return Dirs {outOf=csrc, bakOf=cbak, srcLoc=AtOut}
        | csrc == cbak =
            return Dirs {bakOf=csrc, outOf=cout, srcLoc=AtBak}
        | otherwise =
            filtOutput level Info "The source must be the same as the backup or the output. Otherwise, the backup is redundant\n" >> exitFailure


sanitizeOptions :: Options -> IO SPPOpts
sanitizeOptions opts
        | clean opts    = case (rawDirectiveStart opts, rawNoCleanOnErrors opts) of
            (Nothing, False) -> Clean (rawPrintLevel opts) <$> extractDirs opts
            (Just _, _) -> errorDie (rawPrintLevel opts) "--directive-start should not be used with --clean"
            (_, True) -> errorDie (rawPrintLevel opts) "--no-clean-on-errors should not be used with --clean"
        | otherwise     = do
                    dirse <- extractDirs opts
                    return $ Preprocess (rawPrintLevel opts) dirse (fromMaybe "" (rawDirectiveStart opts)) (rawNoCleanOnErrors opts)

rawPrintLevel :: Options -> PrintLevel
rawPrintLevel opts
    | isDebug opts      = Debug
    | isVerbose opts    = Verbose
    | otherwise         = Info

extractDirs :: Options -> IO Dirs
extractDirs opts = extractRawDirs opts >>= checkDirs (rawPrintLevel opts)

errorDie :: PrintLevel -> String -> IO a
errorDie level str = filtOutput level Info (str ++ "\n") >> exitFailure >> return undefined

extractRawDirs :: Options -> IO RawDirs
extractRawDirs opts = fromSrcOutBak (rawSrcDir opts) (rawOutDir opts) (rawBakDir opts)
    where
    fromSrcOutBak src (Just out) (Just bak)
        | out == bak    = errorDie (rawPrintLevel opts) "The backup and output directories cannot be the same"
        | otherwise     = return RawDirs {rawSrc=src, rawOut=out, rawBak=bak}
    fromSrcOutBak src Nothing (Just bak)
        = fromSrcOutBak src (Just src) (Just bak)
    fromSrcOutBak src (Just out) Nothing
        = fromSrcOutBak src (Just out) (Just src)
    fromSrcOutBak src Nothing Nothing
        = fromSrcOutBak src (Just src) (Just $ src ++ ".bak")


magicallyNothing :: String
magicallyNothing = "\x01"

optionParser :: ParserSpec Options
optionParser = Options `parsedBy`
        boolFlag "clean" `andBy`
        reqFlag "src" `andMaybeBy`
        optFlag magicallyNothing "out" `andMaybeBy`
        optFlag magicallyNothing "bak" `andMaybeBy`
        optFlag magicallyNothing "directive-start" `andBy`
        boolFlag "no-clean-on-errors" `andBy`
        boolFlag "debug" `andBy`
        boolFlag "verbose"

infixl 1 `andMaybeBy`

andMaybeBy :: ParamSpec spec => ParserSpec (Maybe String -> b) -> spec String -> ParserSpec b
andMaybeBy prev spec = fmap (.isMagicallyNothing) prev `andBy` spec
    where
    isMagicallyNothing x
        | x == magicallyNothing    = Nothing
        | otherwise             = Just x

cleanCanon :: FilePath -> IO FilePath
cleanCanon path = do
    exists <- doesDirectoryExist path
    unless exists $ createDirectoryIfMissing False path
    canon <- canonicalizePath path
    unless exists $ removeDirectory path
    return canon

data PrintLevel = Info | Debug | Verbose
    deriving (Eq, Ord)

output :: SPPOpts -> PrintLevel -> String -> IO ()
output opts level toPrint
    = when (level <= printLevel opts) $ putStr toPrint

filtOutput :: PrintLevel -> PrintLevel -> String -> IO ()
filtOutput deflevel level toPrint
    = when (level <= deflevel) $ putStr toPrint
