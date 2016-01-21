{-# LANGUAGE DoAndIfThenElse #-}
module Interface.Args (
        Dirs (..),
        SPPOpts(..),
        processArguments
    ) where

import System.Console.ArgParser
import System.Console.ArgParser.Parser
import Control.Applicative

import System.Directory
import System.Exit

data Dirs = Dirs {
    srcDir :: String,
    outDir :: String,
    bakDir :: String
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
processArguments program = withParseResult optionParser $ \opts ->
    case checkDirs <$> sanitizeOptions opts of
        (Left err) -> putStrLn "Command line argument mismatch error" >> putStrLn err
        (Right sppopts) -> sppopts >>= program

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

checkDirs :: SPPOpts -> IO SPPOpts
checkDirs opts = case dirs opts of
    Dirs src out bak -> do
        csrc <- canonicalizePath src
        cout <- canonicalizePath out
        cbak <- canonicalizePath bak
        if cout == cbak then
            putStrLn "The backup and output directories cannot be the same" >> exitFailure
        else
            return $ opts {dirs = Dirs csrc cout cbak}

sanitizeOptions :: Options -> Either String SPPOpts
sanitizeOptions opts
        | clean opts    = case (rawDirectiveStart opts, rawNoCleanOnErrors opts) of
            (Nothing, False) -> Clean <$> extractDirs opts
            (Just _, _) -> Left "--directive-start should not be used with --clean"
            (_, True) -> Left "--no-clean-on-errors should not be used with --clean"
        | otherwise     = do
            dirse <- extractDirs opts
            return $ Preprocess dirse (maybe "" id (rawDirectiveStart opts)) (rawNoCleanOnErrors opts)

extractDirs :: Options -> Either String Dirs
extractDirs opts = fromSrcOutBak (rawSrcDir opts) (rawOutDir opts) (rawBakDir opts)
    where
    fromSrcOutBak src (Just out) (Just bak)
        | out == bak    = Left $ "The backup and output directories cannot be the same"
        | otherwise     = Right $ Dirs src out bak
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
