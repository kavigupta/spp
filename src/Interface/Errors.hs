module Interface.Errors (
        SPPError(..),
            concatErrors, isError
    ) where

import Text.Parsec
import Control.Exception

import Data.Monoid

type Input = String

data SPPError
    = DirectiveError Input ParseError
    | InvalidCommand Input ParseError
    | IOError IOException
    | IncludeError ParseError
    | WriteOutError ParseError
    | ErrorSequence [SPPError]
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
isError = (== ErrorSequence [])
