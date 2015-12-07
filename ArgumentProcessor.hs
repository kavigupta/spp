module ArgumentProcessor(
        Options
            srcDir, nameRegex, nameReplacement,
        optionParser
    ) where

import System.Console.ArgParser.Run
import System.Environment
import System.Exit

data Options = Options {
        srcDir :: String,
        nameRegex :: String,
        nameReplacement :: String,
    }

optionParser :: ParserSpec Options
optionParser = Options `parsedBy`
    optFlag "." "src" `andBy`
    optFlag  "(.+)\\.gpp" "regex" `andBy`
    optFlag "\\1" "replacement"
