module Interface.Errors (
        SPPError(..),
            concatErrors
    ) where

import Text.Parsec
import Control.Exception

type Input = String

data SPPError = DirectiveError Input ParseError
    | InvalidCommand Input ParseError
    | IOError IOException
    | IncludeError ParseError
    | WriteOutError ParseError
        deriving Show

{-
Collects all `Left` errors from the given list of Eithers
-}
concatErrors :: [Either a b] -> Maybe a
concatErrors [] = Nothing
concatErrors (x:xs) = either Just (const $ concatErrors xs) x
