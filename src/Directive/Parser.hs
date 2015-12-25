module Directive.Parser (
        parseCommand, Command(..)
    ) where

import Text.Parsec

import Interface.Errors
import Tools.Parser

data Command =
    Replace String String |
    Exec String |
    PassThrough String |
    DoWrite |
    DoInclude
        deriving Show

parseCommand :: String -> Either SPPError Command
parseCommand input
    = case doParse command input of
        Left err -> Left $ InvalidCommand input err
        Right x -> Right x

command :: Parser Command
command = do
    spaces
    replace <|> exec <|> passThrough <|> write <|> include

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
