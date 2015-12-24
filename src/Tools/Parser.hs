module Tools.Parser(
    Parser,
        haskellString, spacedString, doParse
) where

import Text.Parsec.Language (haskellDef)
import Text.Parsec.Token
import Text.Parsec

import Data.Functor.Identity

type Parser x = ParsecT String String Identity x

haskellString :: Parser String
haskellString = stringLiteral (makeTokenParser haskellDef)

spacedString :: String -> Parser ()
spacedString str = spaces >> string str >> spaces

doParse :: Parser a -> String -> Either ParseError a
doParse parser input = runIdentity $ runParserT parser "(unknown)" "" input
