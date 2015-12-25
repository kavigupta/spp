module Interface.Errors (
        IOExcHandler, ParseHandler,
        SPPError(..),
            concatErrors, isError
    ) where

import Text.Parsec
import Control.Exception

import Data.Monoid

type Input = String

type IOExcHandler = IOException -> SPPError
type ParseHandler = ParseError -> SPPError

data SPPError
    = DirectiveError        Input ParseError
    | InvalidCommand        Input ParseError
    | IncludeIOError        Input IOException
    | PassThroughError      Input String IOException
    | WriteIOError          Input IOException
    | IncludeParseError     Input ParseError
    | WriteParseError       Input ParseError
    | BackupExistsError     FilePath
    | BackupError           FilePath IOException
    | ExecError             String IOException
    | ErrorSequence         [SPPError]
        deriving (Show, Eq)

instance Monoid SPPError where
    mempty = ErrorSequence []
    (ErrorSequence a) `mappend` (ErrorSequence b)
        = ErrorSequence $ a ++ b
    (ErrorSequence a) `mappend` b
        = ErrorSequence $ a ++ [b]
    a `mappend` (ErrorSequence b)
        = ErrorSequence $ a : b
    a `mappend` b = ErrorSequence [a, b]
{-
Collects all `Left` errors from the given list of Eithers
-}
concatErrors :: (Monoid a) => [Either a b] -> a
concatErrors = mconcat . map getError
    where
    getError (Left err) = err
    getError (Right _) = mempty

isError :: SPPError -> Bool
isError = (/= ErrorSequence [])

--backupExistsError = "A backup already exists. Perhaps you forgot to run spp --clean last time"
