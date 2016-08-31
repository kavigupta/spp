module Interface.Errors (
        IOExcHandler, ParseHandler,
        SPPError(..), ErrorType(..),
            sppError, concatErrors, actualError,
            RestoreSituation(..)
    ) where

import Text.Parsec
import Control.Exception

import Data.List
import Data.String.Utils

type Input = String

type IOExcHandler = IOException -> SPPError
type ParseHandler = ParseError -> SPPError

sppError :: (Show err) => ErrorType -> FilePath -> Maybe Input -> err -> SPPError
sppError ty pat inp = SPPError ty pat inp . Just . show

data SPPError
    = SPPError ErrorType FilePath (Maybe Input) (Maybe String)
    | ErrorSequence [SPPError]
        deriving Eq

data ErrorType
    = BackupExistsError
    | OutputExistsError
    | BackupError
    | RestoreError RestoreSituation
    | InvalidDirectiveList
    | InvalidDirective
    | IncludeIOError
    | IncludeParseError
    | ReadIOError
    | WriteIOError
    | WriteParseError
    | PassThroughError String
    | ExecError String
    | CircularDependencyError [FilePath]
        deriving Eq

data RestoreSituation = UponRequest | UponFailure
    deriving Eq

tabify :: String -> String
tabify = ("\t"++) . replace "\n" "\n\t"

instance Show SPPError where
    show (SPPError errtype path input exc)
            = "An error occured in processing " ++ path ++ "\n"
                ++ "\t" ++ show errtype ++ "\n"
                ++ excLine
                ++ inLine
        where
        excLine = case exc of
            Nothing -> ""
            Just err -> "\t" ++ err ++ "\n"
        inLine = case input of
            Nothing -> ""
            Just inp -> tabify inp
    show (ErrorSequence errs)
        = "Multiple errors occured\n"
            ++ tabify (intercalate "\n" $ map show errs)

instance Show ErrorType where
    show BackupExistsError = "Backups exist; perhaps you forgot to run --clean last time"
    show OutputExistsError = "The given output folder would be overwritten by your changes. Perhaps you are running spp twice without clean"
    show BackupError = "An error occured in creating backups"
    show (RestoreError UponRequest) = "Backups do not exist or are irrecoverable; "
        ++ "either you have already --clean or there is some other serious problem"
    show (RestoreError UponFailure) = "Backups do not exist or are irrecoverable; "
        ++ "Since this is supposed to be an error recovery step after that last error, "
        ++ "I'm afraid I'm not entirely sure what to do at this point."
    show InvalidDirectiveList = "Processing a list of directives failed due to invalid syntax"
    show InvalidDirective = "Processing a directive failed due to invalid syntax"
    show IncludeIOError = "Including a file failed due to an IO error"
    show WriteIOError = "Writing a file failed due to an IO error"
    show ReadIOError = "Reading a file failed due to an IO error"
    show (PassThroughError cmd) = "Passing a file through " ++ show cmd ++ " failed"
    show IncludeParseError = "Including a file failed due to invalid syntax"
    show WriteParseError = "Writing to a file failed due to a parse error"
    show (ExecError cmd) = "Executing " ++ show cmd ++ " failed"
    show (CircularDependencyError deps) = "Circular dependencies: " ++ intercalate " -> " deps


instance Monoid SPPError where
    mempty = ErrorSequence []
    (ErrorSequence a) `mappend` (ErrorSequence b)
        = ErrorSequence $ a ++ b
    (ErrorSequence a) `mappend` b
        = ErrorSequence $ a ++ [b]
    a `mappend` (ErrorSequence b)
        = ErrorSequence $ a : b
    a `mappend` b = ErrorSequence [a, b]
{-
Collects all `Left` errors from the given list of Eithers
-}
concatErrors :: (Monoid a) => [Either a b] -> a
concatErrors = mconcat . map getError
    where
    getError (Left err) = err
    getError (Right _) = mempty

actualError :: SPPError -> Either SPPError ()
actualError (ErrorSequence []) = Right ()
actualError x = Left x

--backupExistsError = Just err
