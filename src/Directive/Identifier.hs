module Directive.Identifier (identifyDirectives) where

import Directive.Parser

import Text.Parsec.Token

-- | Represents the directives along with the rest of the file
data Directives = Directives
    [String] -- ^ The directives, in string form
    String -- ^ The rest of the file

-- takes a string containing a beginning of line condition and outputs a parser that parses directives
directives :: String -> Parser Directives
directives start = try (directivesIfExist start) <|> directivesIfNotExist

directivesIfExist :: String -> Parser Directives
directivesIfExist start = do
    string "preprocess:"
    endOfLine
    dirs <- many directive
    rest <- many anyChar
    return $ Directives dirs rest

directivesIfNotExist :: String -> Parser Directives
directivesIfNotExist start
    = liftM (Directives []) $ many anyChar

directive :: String -> Parser String
directive start = do
    spaces
    string start
    manyTill anyChar (try endOfLine)
