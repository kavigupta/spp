module ArgumentProcessor(
        Options,
            srcDir, clean,
        optionParser
    ) where

import System.Console.ArgParser

data Options = Options {
        clean :: Bool,
        srcDir :: String
    }

optionParser :: ParserSpec Options
optionParser = Options `parsedBy`
    boolFlag "clean" `andBy`
    optFlag "." "src"
