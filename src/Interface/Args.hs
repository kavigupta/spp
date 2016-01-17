module Interface.Args (
        Options,
            srcDir, outDir, bakDir, clean, directiveStart, noCleanOnErrors,
        optionParser
    ) where

import System.Console.ArgParser
import System.Console.ArgParser.Parser

data Options = Options {
        -- Whether or not this is a cleaning run
        clean :: Bool,
        -- The source directory to preprocess or clean
        srcDir :: String,
        -- Where the output files should go.
        outDir :: Maybe String,
        -- Where the backup files should go.
        bakDir :: Maybe String,
        -- The string to start directives with
        directiveStart :: String,
        -- Clean up on errors
        noCleanOnErrors :: Bool
    }

--TODO add update support

nothingString :: String
nothingString = "\x01"

optionParser :: ParserSpec Options
optionParser = Options `parsedBy`
    boolFlag "clean" `andBy`
    reqFlag "src" `andMaybeBy`
    optFlag nothingString "out" `andMaybeBy`
    optFlag nothingString "bak" `andBy`
    optFlag "" "directive-start" `andBy`
    boolFlag "no-clean-on-errors"

infixl 1 `andMaybeBy`

andMaybeBy :: ParamSpec spec => ParserSpec (Maybe String -> b) -> spec String -> ParserSpec b
andMaybeBy prev spec = fmap (.isNothing) prev `andBy` spec
    where
    isNothing x
        | x == nothingString    = Nothing
        | otherwise             = Just x
