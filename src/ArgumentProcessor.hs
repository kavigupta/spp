module ArgumentProcessor(
        Options,
            srcDir, clean,
        optionParser
    ) where

import System.Console.ArgParser

data Options = Options {
        -- Whether or not this is a cleaning run
        clean :: Bool,
        -- The source directory to preprocess or clean
        srcDir :: String
    }

optionParser :: ParserSpec Options
optionParser = Options `parsedBy`
    boolFlag "clean" `andBy`
    reqFlag "src"
