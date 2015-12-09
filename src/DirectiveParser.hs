module DirectiveParser (
        doParse, parseCommand, Command(..), Parser, spacedString, haskellString
    ) where

import Text.Parsec.Language (haskellDef)
import Text.Parsec.Token
import Text.Parsec
import Data.Functor.Identity


data Command =
    Replace String String |
    Exec String |
    PassThrough String |
    DoWrite |
    DoInclude

type Parser x = ParsecT String String Data.Functor.Identity.Identity x


parseCommand :: String -> Either String Command
parseCommand input
    = case doParse command input of
        Left err -> Left $ "Invalid command " ++ input ++ "\n" ++ show err
        Right x -> Right x

command :: Parser Command
command = do
    spaces
    replace <|> exec <|> passThrough <|> write <|> include

haskellString :: Parser String
haskellString = stringLiteral (makeTokenParser haskellDef)

spacedString :: String -> Parser ()
spacedString str = spaces >> string str >> spaces

replace :: Parser Command
replace = do
    spacedString "replace"
    regex <- haskellString
    spacedString "->"
    replacement <- haskellString
    return $ Replace regex replacement

exec :: Parser Command
exec = do
    spacedString "exec"
    toexec <- many anyChar
    return $ Exec toexec

passThrough :: Parser Command
passThrough = do
    spacedString "pass"
    toPass <- many anyChar
    return $ PassThrough toPass

write :: Parser Command
write = spacedString "writeout" >> return DoWrite

include :: Parser Command
include = spacedString "include" >> return DoInclude

doParse :: Parser a -> String -> Either ParseError a
doParse parser input = runIdentity $ runParserT parser "(unknown)" "" input
