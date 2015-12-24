module Directive.Identifier (identifyDirectives) where

import Directive.Parser

import Text.Parsec.Token

-- | Represents the directives along with the rest of the file
data Directives = Directives
    [String] -- ^ The directives, in string form
    String -- ^ The rest of the file

-- takes a string containing
directives :: String -> Parser Directives
direcitves start = do
    string "preprocess:"
    endOfLine
    dirs <- many directive
    rest <- many anyChar
    return $ Directives dirs rest

directive :: String -> Parser String
directive start = do
    spaces
    string start
    manyTill anyChar (try endOfLine)
