{-# LANGUAGE TupleSections #-}
module Directive.Identifier (parseDirectives) where

import CommandGenerator
import Tools.Parser

import Text.Parsec

import Control.Monad
import Control.Applicative((<$>))

-- | Represents the directives along with the rest of the file
data Directives = Directives
    String -- ^ The first line, in string form
    [String] -- ^ The directives, in string form
    String -- ^ The rest of the file

parseDirectives :: String -> String -> String -> Either String (String, [Action], String)
parseDirectives path start input
    = case doParse (directives start) input of
        (Left err) -> Left $ "Invalid Directive; error " ++ show err ++ " encountered when processing\n" ++ input
        (Right (Directives header dirs rest)) -> (header,,rest) <$> mapM (toCommand path) dirs


-- takes a string containing a beginning of line condition and outputs a parser that parses directives
directives :: String -> Parser Directives
directives start = try (directivesIfExist start) <|> directivesIfNotExist

directivesIfExist :: String -> Parser Directives
directivesIfExist start = directivesIfStart start "" <|> secondLine
    where
    secondLine = manyTill anyChar (try endOfLine) >>= directivesIfStart start

directivesIfStart :: String -> String -> Parser Directives
directivesIfStart start firstLine = do
    string "preprocess"
    try $ char ':'
    endOfLine
    dirs <- many (directive start)
    rest <- many anyChar
    return $ Directives firstLine dirs rest

directivesIfNotExist :: Parser Directives
directivesIfNotExist
    = liftM (Directives "" []) $ many anyChar

directive :: String -> Parser String
directive start = do
    spaces
    string start
    manyTill anyChar (try endOfLine)
