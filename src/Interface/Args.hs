module Interface.Args (
        Options,
            srcDir, clean, directiveStart, noCleanOnErrors,
        optionParser
    ) where

import System.Console.ArgParser

data Options = Options {
        -- Whether or not this is a cleaning run
        clean :: Bool,
        -- The source directory to preprocess or clean
        srcDir :: String,
        -- The string to start directives with
        directiveStart :: String,
        -- Clean up on errors
        noCleanOnErrors :: Bool
    }

--TODO Add support for specifying an output directory
--TODO add update support

optionParser :: ParserSpec Options
optionParser = Options `parsedBy`
    boolFlag "clean" `andBy`
    reqFlag "src" `andBy`
    optFlag "" "directive-start" `andBy`
    boolFlag "no-clean-on-errors"
