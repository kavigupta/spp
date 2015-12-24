module ArgumentProcessor(
        Options,
            srcDir, clean, directiveStart,
        optionParser
    ) where

import System.Console.ArgParser

data Options = Options {
        -- Whether or not this is a cleaning run
        clean :: Bool,
        -- The source directory to preprocess or clean
        srcDir :: String,
        -- The string to start directives with, note this is a regex
        directiveStart :: String
    }

optionParser :: ParserSpec Options
optionParser = Options `parsedBy`
    boolFlag "clean" `andBy`
    reqFlag "src" `andBy`
    optFlag "" "directive-start"
