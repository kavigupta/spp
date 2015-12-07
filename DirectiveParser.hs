module DirectiveParser (toCommand) where

import Text.Parsec.Language (haskellDef)
import Text.Parsec.Token
import Text.Parsec
import Control.Applicative hiding ((<|>))
import Data.Functor.Identity
import Text.Regex

data Command =
    Replace String String |
    CodeDump String String

type Parser x = ParsecT String String Data.Functor.Identity.Identity x

toCommand :: String -> Either String (String -> IO String)
toCommand str = getCommand <$> parseCommand str

getCommand :: Command -> String -> IO String
getCommand (Replace regex replacement)= return . replacer
    where
    replacer x = subRegex (mkRegex regex) x replacement
getCommand (CodeDump _ _) = return

parseCommand :: String -> Either String Command
parseCommand input
    = case runIdentity $ runParserT command "(unknown)" "" input of
        Left err -> Left $ "Invalid command " ++ input ++ "\n" ++ show err
        Right x -> Right x

command :: Parser Command
command = do
    spaces
    replace <|> codeDump

haskellString :: Parser String
haskellString = stringLiteral (makeTokenParser haskellDef)

replace :: Parser Command
replace = do
    string "replace:"
    spaces
    regex <- haskellString
    spaces
    string "->"
    spaces
    replacement <- haskellString
    return $ Replace regex replacement

codeDump :: Parser Command
codeDump = do
    string "code-dump:"
    spaces
    string "language-name"
    spaces
    string "="
    spaces
    name <- haskellString
    spaces
    string "language-ext"
    spaces
    string "="
    spaces
    ext <- haskellString
    return $ CodeDump name ext
