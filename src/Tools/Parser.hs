module Tools.Parser(
    Parser,
        haskellString, spacedString, doParse, nonNewlineSpace
) where

import Text.Parsec.Language (haskellDef)
import Text.Parsec.Token
import Text.Parsec

import Data.Functor.Identity

type Parser x = ParsecT String String Identity x

haskellString :: Parser String
haskellString = stringLiteral (makeTokenParser haskellDef)

spacedString :: String -> Parser ()
spacedString str = skipMany nonNewlineSpace >> string str >> skipMany nonNewlineSpace

doParse :: Parser a -> String -> Either ParseError a
doParse parser input = runIdentity $ runParserT parser "(unknown)" "" input

nonNewlineSpace :: Parser Char
nonNewlineSpace = oneOf " \t"
