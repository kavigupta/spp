{-# LANGUAGE TupleSections #-}
module Directive.Identifier (identifyDirectives) where

import Directive.Parser

import Text.Parsec.Token

import Control.Applicative

-- | Represents the directives along with the rest of the file
data Directives = Directives
    String -- ^ The first line, in string form
    [String] -- ^ The directives, in string form
    String -- ^ The rest of the file

parseDirectives :: String -> String -> Either String ([Command], String)
parseDirectives start input
    = case doParse input directives of
        (Left err) -> Left $ "Invalid Directive; error " ++ show err ++ " encountered when processing\n" ++ input
        (Right (Directives dirs rest)) -> (,rest) <$> mapM parseCommand dirs


-- takes a string containing a beginning of line condition and outputs a parser that parses directives
directives :: String -> Parser Directives
directives start = try (directivesIfExist start) <|> directivesIfNotExist

directivesIfExist :: String -> Parser Directives
directivesIfExist start = directivesIfStart start <|> (manyTill anyChar (try endOfLine)) >>= directivesIfStart start

directivesIfStart :: String -> String -> Parser Directives
directivesIfStart start firstLine = do
    string "preprocess"
    try $ char ':'
    endOfLine
    dirs <- many directive
    rest <- many anyChar
    return $ Directives firstLine dirs rest

directivesIfNotExist :: String -> Parser Directives
directivesIfNotExist start
    = liftM (Directives "" []) $ many anyChar

directive :: String -> Parser String
directive start = do
    spaces
    string start
    manyTill anyChar (try endOfLine)
