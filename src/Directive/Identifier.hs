{-# LANGUAGE TupleSections #-}
module Directive.Identifier (
        parseDirectives,
        Directives(..)
    ) where

import Directive.Parser

import Tools.Parser
import Interface.Errors

import Text.Parsec

import Control.Monad
import Control.Applicative((<$>))

-- | Represents the directives along with the rest of the file
data Directives a = Directives
    String -- ^ The first line, in string form
    [a] -- ^ The directives
    String -- ^ The rest of the file
        deriving Show

parseDirectives :: String -> String -> Either SPPError (Directives Command)
parseDirectives start input
    = case doParse (directives start) input of
        (Left err) -> Left $ DirectiveError input err
        (Right (Directives header dirs rest)) -> (\commands -> Directives header commands rest) <$> mapM parseCommand dirs


-- takes a string containing a beginning of line condition and outputs a parser that parses directives
directives :: String -> Parser (Directives String)
directives start = try (directivesIfExist start) <|> directivesIfNotExist

directivesIfExist :: String -> Parser (Directives String)
directivesIfExist start = try (directivesIfStart start "") <|> secondLine
    where
    secondLine = manyTill anyChar (try endOfLine) >>= directivesIfStart start

directivesIfStart :: String -> String -> Parser (Directives String)
directivesIfStart start firstLine = do
    string "preprocess"
    try $ char ':'
    endOfLine
    dirs <- many (directive start)
    rest <- many anyChar
    return $ Directives firstLine dirs rest

directivesIfNotExist :: Parser (Directives String)
directivesIfNotExist
    = liftM (Directives "" []) $ many anyChar

directive :: String -> Parser String
directive start = do
    nonNewlineSpace
    many nonNewlineSpace
    string start
    manyTill anyChar (try endOfLine)
