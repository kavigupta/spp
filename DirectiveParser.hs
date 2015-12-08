module DirectiveParser (toCommand) where

import Text.Parsec.Language (haskellDef)
import Text.Parsec.Token
import Text.Parsec
import Control.Applicative hiding ((<|>), many)
import Data.Functor.Identity
import Text.Regex
import ShellHandler
import System.FilePath

data Command =
    Replace String String |
    Exec String |
    PassThrough String

type Parser x = ParsecT String String Data.Functor.Identity.Identity x

toCommand :: FilePath -> String -> Either String (String -> IO String)
toCommand path str = getCommand path <$> parseCommand str

getCommand :: FilePath -> Command -> String -> IO String
getCommand _ (Replace regex replacement) u = return . replacer $ u
    where
    replacer x = subRegex (mkRegex regex) x replacement
getCommand path (Exec toExec) x = systemInDir (takeDirectory path) toExec >> return x
getCommand path (PassThrough toPass) str = readProcessInDir path toPass str

parseCommand :: String -> Either String Command
parseCommand input
    = case runIdentity $ runParserT command "(unknown)" "" input of
        Left err -> Left $ "Invalid command " ++ input ++ "\n" ++ show err
        Right x -> Right x

command :: Parser Command
command = do
    spaces
    replace <|> exec <|> passThrough

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
