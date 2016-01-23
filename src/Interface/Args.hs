{-# LANGUAGE DoAndIfThenElse #-}
module Interface.Args (
        Dirs (..),
        SourceLocation(..),
        SPPOpts(..),
        processArguments
    ) where

import System.Console.ArgParser
import System.Console.ArgParser.Parser
import Control.Applicative

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
        dirs :: Dirs
    } |
    Preprocess {
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
    rawNoCleanOnErrors :: Bool
}

--TODO add update support

checkDirs :: RawDirs -> IO Dirs
checkDirs (RawDirs {rawSrc=src,rawOut=out,rawBak=bak}) = do
        putStrLn $ "abcdef " ++ bak
        csrc <- cleanCanon src
        cout <- cleanCanon out
        cbak <- cleanCanon bak
        print (src, out, bak)
        u <- fromCanonicalTriple csrc cout cbak
        print u
        return u
    where
    fromCanonicalTriple :: String -> String -> String -> IO Dirs
    fromCanonicalTriple csrc cout cbak
        | cout == cbak =
            putStrLn "The backup and output directories cannot be the same" >> exitFailure
        | csrc == cout =
            return Dirs {outOf=csrc, bakOf=cbak, srcLoc=AtOut}
        | csrc == cbak =
            return Dirs {bakOf=csrc, outOf=cout, srcLoc=AtBak}
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
                    return $ Preprocess dirse (fromMaybe "" (rawDirectiveStart opts)) (rawNoCleanOnErrors opts)

extractDirs :: Options -> IO Dirs
extractDirs opts = extractRawDirs opts >>= checkDirs

errorDie :: String -> IO a
errorDie str = putStrLn str >> exitFailure >> return undefined

extractRawDirs :: Options -> IO RawDirs
extractRawDirs opts = fromSrcOutBak (rawSrcDir opts) (rawOutDir opts) (rawBakDir opts)
    where
    fromSrcOutBak src (Just out) (Just bak)
        | out == bak    = errorDie "The backup and output directories cannot be the same"
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
        boolFlag "no-clean-on-errors"

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
